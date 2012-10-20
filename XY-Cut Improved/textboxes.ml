(*
let img_size = (100, 100)
let img_size_x = let (x, y) = img_size in x
let img_size_y = let (x, y) = img_size in y
*)

(* Returns a boolean equals to true if the pixel is black *)

let is_pixel_black x y =
    let color = Sdlvideo.get_pixel_color img x y in
        color = (0, 0, 0)
;;

(* Returns the number of black pixels in the line between the two x bounds *)

let rec x_hist y x1 x2 = match x1 with
    | x2 -> 0
    | x -> (if (is_pixel_black x y) then 1 else 0)
        + x_hist y (x+1) x2
;;

(* Returns the number of black pixels in the column between the two y bounds *)

let rec y_hist x y1 y2 = match y1 with
    | y2 -> 0
    | y -> (if (is_pixel_black x y) then 1 else 0)
        + y_hist x (y+1) y2
;;


(* Returns the a list of quadra-int (x1, x2, y1, y2) formed by horizontal cuts
in the given box *)

Type  quadra_tree = Empty | Node of (int, int, int, int) * quadra_tree list
;;

let boxes_tree = ref (x1, x2, y1, y2)
val boxes_tree : quadra_tree ref = {contents = (x1, x2, y1, y2)}

let tree_list = ref []
val tree_list : quadra_tree list ref = {contents = []}


(* chapeau *)
let x_cut bounds =
    let tree = create_tree_list() in
    begin

x_cut_tree (bounds, tree);
        tree;
    end ;;

(* normale *)
let x_cut_tree ((x1, x2, y1, y2), tree_list) =
    let tmp_y1 = (-1) in

let y = ref y1 in

begin

while (y <= y2 + 1) do

if (y = y2 +1 || x_hist (y, x1, x2) = 0) (* White line or out of bounds *)
            then
                if (tmp_y1 <> -1) then
                begin

                    boxes_tree := ((x1, x2, y1, y2), y_cut (x1, x2, tmp_y1, y) :: !tree_list);
                    tmp_y1 := -1;

end
            else
                if (tmp_y1 = -1) then
                    tmp_y1 := y;

done
        y := y+1
end ;;
