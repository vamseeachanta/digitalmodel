IMPORTXYZ : dialog {
            label = "ImportXYZ - By JefferyPSanders.com";
            : column {
              : column {
                : boxed_row { 
                  : edit_box {
                    key = "filename";
                    label = "File Name:";
                    edit_width = 30;
                    value = "";
                  }                  
                  : button {
                    key = "getfile";
                    label = "Browse for File";
                    is_default = false;
                    is_cancel = false;
                  }
                }
                : boxed_row {
                  : popup_list {
                    key = "decplaces";
                    label = "Decimal Places to Display:";
                    edit_width = 10;
                    value = "4";
                  }
                  spacer;
                  spacer;
                  spacer;
                  spacer;   
                  spacer;
                  spacer;        
                  : button {
                    key = "getdata";
                    label = "Get XYZ Coords";
                    is_default = false;
                    is_cancel = false;
                  }
                }
                : row {  
                  : list_box {
                    key = "xyz";
                    label = "Showing first 50 results only:";
                    multiple_select = "FALSE";
                    fixed_width_font = true;
                  }
                }
              }
              : row {
                : image {
                  key = "progbar";
                  width = 50;                  
                  height = 1.59;
                  fixed_width = false;
                  fixed_height = true;
                  aspect_ratio = 1;
                  color = -15;
                  vertical_margin = none;
                }
              }
              : row {
                 : boxed_column {
                  : radio_column {
                    : radio_button {
                      label = "Draw a Node";
                      key = "dnode";
                      value  = "1";
                    }
                    : radio_button {
                      label = "Draw a Circle";
                      key = "dcirc";
                      value  = "0";
                    }
                    : radio_button {
                      label = "Draw Lines";
                      key = "dline";
                      value  = "0";
                    }
                    : radio_button {
                      label = "Insert a Block";
                      key = "dbloc";
                      value  = "0";
                    }
                    : radio_button {
                      label = "Edit Attribute";
                      key = "dattr";
                      value  = "0";
                    }
                    : radio_button {
                      label = "Insert Excel Note";
                      key = "dnote";
                      value  = "0";
                    }
                  }
                }
                : boxed_column {
                  : popup_list {
                    key = "layers";
                    label = "Layer Name:";
                    multiple_select = "FALSE";
                  }
                  : edit_box {
                    key = "cdia";
                    label = "Diameter:";
                    edit_width = 15;
                    value = "0.1";
                  }           
                  : row {
                    : popup_list {
                      key = "blocks";
                      label = "Block Name";
                      edit_width = 15;
                      value = "";
                    }  
                    : button {
                      key = "getblkname";
                      label = "Browse";
                      is_default = false;
                      is_cancel = false;
                    }
                  }
                  : edit_box {
                    key = "xtag";
                    label = "X Tag:";
                    edit_width = 15;
                    value = "";
                  }           
                  : edit_box {
                    key = "ytag";
                    label = "Y Tag:";
                    edit_width = 15;
                    value = "";
                  }           
                  : edit_box {
                    key = "ztag";
                    label = "Z Tag:";
                    edit_width = 15;
                    value = "";
                  }           
                }
              }
              : row {
                : boxed_row {
                  : button {
                    key = "accept";
                    label = "  Okay  ";
                    is_default = true;
                  }
                  : button {
                    key = "help";
                    label = "  Help  ";
                    is_default = false;
                  }
                  : button {
                    key = "cancel";
                    label = "  Cancel  ";
                    is_default = false;
                    is_cancel = true;
                  }
                }
              }
            }
}
FINDDELIM : dialog {
            label = "Select File Type";
            : column {
              : boxed_column {
                : radio_column {
                  : radio_button {
                    label = "Type 1 -  X enter Y enter... etc.";
                    key = "type7";
                    value  = "1";
                  }
                  : radio_button {
                    label = "Type 2 -  X enter Y enter Z enter... etc.";
                    key = "type8";
                    value  = "0";
                  }
                  : radio_button {
                    label = "Type 3 -  # of characters for each coord";
                    key = "type9";
                    value  = "0";
                  }
                }  
                : edit_box {
                  key = "charnum";
                  label = "Number of Characters:";
                  edit_width = 15;
                  value = "12";
                }           
              }    
              : row {
                : boxed_row {
                  : button {
                    key = "accept";
                    label = "  Okay  ";
                    is_default = true;
                  }
                  : button {
                    key = "cancel";
                    label = "  Cancel  ";
                    is_default = false;
                    is_cancel = true;
                  }
                }
              }
            }
}   
XL_GET : dialog {
         label = "Select EXCEL Sheet Name";
         : column {
           : boxed_column {
             : list_box{
               label ="Available Sheet Names";
               key = "sheetlist";
               height = 3;
               width = 15;
               multiple_select = false;
               fixed_width_font = true;
             }
           }
         }
         : column {
            : boxed_row {
             : button { 
               key = "accept";
               label = " Okay ";
                is_default = true;
             }
             : button { 
               key = "cancel";
               label = "Cancel";
               is_default = false;
               is_cancel = true;
             }
            }
         }
}    
XYZ_HELP : dialog {
         label = "ImportXYZ - Help";
         : column {
           : boxed_column {
             : list_box {
               key = "helplist";
               edit_width = 85;
               width = 85;
             } 
           }
         }
         : column {
            : boxed_row {
             : button { 
               key = "cancel";
               label = "Okay";
               is_default = true;
               is_cancel = true;
             }
            }
         }
}  