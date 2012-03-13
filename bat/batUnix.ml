let open_process_out s = 
  BatIO.OC (Unix.open_process_out s)
