module StrMap = CCMap.Make(CCString)

include StrMap

let of_list_uniq l =
  List.fold_left
    (fun accu_map (key, value) ->
       if StrMap.mem key accu_map then
         raise (Invalid_argument "of_list_uniq")
       else
         StrMap.add key value accu_map)
    StrMap.empty
    l
