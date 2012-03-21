open ExtList

let files_of dir = 
  List.enum (Array.to_list (Sys.readdir dir))
