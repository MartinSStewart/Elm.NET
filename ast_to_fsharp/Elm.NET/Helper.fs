module Helper

open System

let filterMap<'a, 'b> (filterMapFunc: 'a -> 'b Option) (list: 'a List): 'b List =
    List.fold 
        (fun newList value -> 
            let newValue = filterMapFunc value
            match newValue with
            | Some a -> a :: newList
            | None -> newList) 
        []
        list

let flip f a b = f b a