let
    /*  ExpandAll_v2 - function recursively expands all expandable cells in given table
		Author: Denis Sipchenko
		  Date: 2024-07-21
		  Link: https://www.linkedin.com/in/denis-sipchenko/

		NB! This function is rework from ExpandAll
		Author: Chris Webb
		  Date: 2014-05-21
		  Link: https://blog.crossjoin.co.uk/2014/05/21/expanding-all-columns-in-a-table-in-power-query/ 	
    */
    Source =                                        // Define function taking parameters:
        (            TableToExpand   as table		// table 
        , optional v_ColumnNumber    as number		// optional column number
        , optional v_maxExpandings   as number      // optional maximal count of Expandings (for debug purposes)
        ) =>
    let        
		//If the column number is missing, make it 0
		v_ActualColumnNumber = if (v_ColumnNumber=null) then 0 else v_ColumnNumber,
		//Find the column name relating to the column number
		v_ColumnName = Table.ColumnNames(TableToExpand){v_ActualColumnNumber},
		//Is there anything to expand in this column (at least one table type value)?
		v_CanExpandCurrentColumn = not List.IsEmpty(List.Select(Table.Column(TableToExpand, v_ColumnName), each _ is table or _ is record or _ is list)),
		// if column expandable, convert scalars to tables to avoid error
		TableToExpand2 = Table.TransformColumns(TableToExpand, {{v_ColumnName, each 
			if  _  is table 
			or  _  =  null 
			or v_CanExpandCurrentColumn =  false 
			then _ 
			else if _ is record then Table.FromRecords({_})
			else if _ is list   then Table.FromRows( {_} )
			else                     Table.FromRows({{_}},{"_#_temp_Column_Name_#_"})
		}}),
		//Get a list containing all of the values in the column
		l_ColumnContents = Table.Column(TableToExpand2, v_ColumnName),        
		//Iterate over each value in the column and then
		//If the value is of type table get a list of all of the columns in the table
		//Then get a distinct list of all of these column names
		l_ColumnsToExpand = List.Distinct(List.Combine(List.Transform(l_ColumnContents, 
						each if _ is table then Table.ColumnNames(_) else {}))),
		//Append the original column name to the front of each of these column names
		l_New_ColumnNames = List.Transform(l_ColumnsToExpand, each v_ColumnName & "." & _),
		// if we can expand current column, decrement v_Actual_cntExpandings
		v_Actual_cntExpandings = if v_maxExpandings=null          then 1e9 
							else if v_CanExpandCurrentColumn = true then v_maxExpandings-1
							else                                         v_maxExpandings   ,
		//If this column can be expanded, then expand it
		rawExpandedTable = if v_CanExpandCurrentColumn and v_Actual_cntExpandings >= 0
					then   Table.ExpandTableColumn(TableToExpand2, v_ColumnName, l_ColumnsToExpand, l_New_ColumnNames) 
					else                           TableToExpand,
		// Rename back column with unexpandable values (scalars), if it exists
		ExpandedTable = Table.RenameColumns(rawExpandedTable,{{v_ColumnName & "." & "_#_temp_Column_Name_#_", v_ColumnName}}, MissingField.Ignore),            
		//If the column has been expanded then keep the column number the same, otherwise add one to it
		v_Next_ColumnNumber = if v_CanExpandCurrentColumn then v_ActualColumnNumber else v_ActualColumnNumber+1,
		//If the column number is now greater than the number of columns in the table
		//Then return the table as it is
		//Else call the ExpandAll function recursively with the expanded table     
		Result = if 1=1
				and v_Next_ColumnNumber    <= (Table.ColumnCount(ExpandedTable)-1) 
				and v_Actual_cntExpandings >     0
						then ExpandAll_v2(ExpandedTable, v_Next_ColumnNumber, v_Actual_cntExpandings)                      
						else ExpandedTable
    in
		Result
in
    Source
