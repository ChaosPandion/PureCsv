#load "PureCsv.fsx"

open PureCsv

try

    let input = "field1,field2,field3\r\n\"aaa\r\n\",\"bb,b\",\"ccc\"\r\n\"in \"\"quotes\"\"\",2,3\r\n1,2,\r\nzzz,yyy,xxx\r\n1,,3\r\n,,"
       
    let throwIfFalse condition =
        if not condition then
            failwith "assertion failure"                   

    let result = parse input

    let headerTestData = [| 
            0, "field1"
            1, "field2" 
            2, "field3" 
        |]  

    for column, expectation in headerTestData do
        throwIfFalse (result.GetHeader column = expectation) 
         
    let recordHeaderTestData = [| 
            0, "field1", "aaa\r\n" 
            0, "field2", "bb,b" 
            0, "field3", "ccc" 
            1, "field1", "in \"quotes\""
            1, "field2", "2"  
            1, "field3", "3"  
            2, "field1", "1"
            2, "field2", "2"  
            2, "field3", ""    
            3, "field1", "zzz"
            3, "field2", "yyy"  
            3, "field3", "xxx"          
            4, "field1", "1"
            4, "field2", ""  
            4, "field3", "3"            
            5, "field1", ""
            5, "field2", ""  
            5, "field3", ""        
        |]  
     
    for row, header, expectation in recordHeaderTestData do
        throwIfFalse (result.GetRecord (row, header) = expectation) 

    let recordIndexTestData = [| 
            0, 0, "aaa\r\n" 
            0, 1, "bb,b" 
            0, 2, "ccc" 
            1, 0, "in \"quotes\""
            1, 1, "2"  
            1, 2, "3"  
            2, 0, "1"
            2, 1, "2"  
            2, 2, ""    
            3, 0, "zzz"
            3, 1, "yyy"  
            3, 2, "xxx"          
            4, 0, "1"
            4, 1, ""  
            4, 2, "3"            
            5, 0, ""
            5, 1, ""  
            5, 2, ""        
        |]  
     
    for row, column, expectation in recordHeaderTestData do
        throwIfFalse (result.GetRecord (row, column) = expectation) 

with 
| ex -> printfn "%O" ex

System.Console.ReadLine ()