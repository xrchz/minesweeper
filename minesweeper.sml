val randomStream = TextIO.openIn"/dev/random"
fun random_bit() =
  let val c = Option.valOf (TextIO.input1 randomStream)
  in Char.ord c < 128 end

fun weighted_bit d =
  if d < 1 then true
  else if random_bit() then weighted_bit (d-1) else false

datatype contents = Mine | Safe
datatype annotation = Flagged | Pressed | Unpressed
type square = contents * annotation
type board = square Vector.vector Vector.vector

val empty_square = (Safe,Unpressed)

fun mk_board mine_rate size =
  let
    fun mk_square _ =
      let
        val contents = if weighted_bit mine_rate then Mine else Safe
      in (contents,Unpressed) end
    fun mk_row _ = Vector.tabulate(size,mk_square)
    val cols = Vector.tabulate(size,mk_row)
  in cols end

fun on_board size (i,j) =
  0 <= i andalso i < size andalso
  0 <= j andalso j < size

fun mk_board_sub board =
  let
    val size = Vector.length board
    fun s (i,j) =
      if on_board size (i,j)
      then Vector.sub(Vector.sub(board,i),j)
      else empty_square
  in s end

val mine_rate = 2
val size = 5
val board = mk_board mine_rate size

val board_sub = mk_board_sub board

fun square_string n (c,Flagged) = "!"
  | square_string n (c,Unpressed) = "+"
  | square_string n (Mine,Pressed) = "*"
  | square_string n (Safe,Pressed) = if n = 0 then " " else Int.toString n

fun revealed_square_string _ ((Mine,_):square) = "*"
  | revealed_square_string _ (Safe,_) = "+"

fun is_Mine (Mine,_) = true
  | is_Mine _ = false

fun is_Pressed (_,Pressed) = true
  | is_Pressed _ = false

fun neighbour_coords size (i,j) =
  List.filter (on_board size)
    [(i-1,j-1), (i,j-1), (i+1,j-1),
     (i-1,j  ),          (i+1,j  ),
     (i-1,j+1), (i,j+1), (i+1,j+1)]

fun num_mines board (i,j) =
  List.length
    (List.filter is_Mine
      (List.map (fn (i,j) => Vector.sub(Vector.sub(board,i),j))
       (neighbour_coords (Vector.length board) (i,j))))

fun mk_board_strings square_func board =
  Vector.mapi(fn(i,row) =>
    Vector.mapi(fn(j,sq) =>
      square_func (num_mines board (i,j)) sq)
    row)
  board

val board_strings = mk_board_strings square_string
val revealed_board_strings = mk_board_strings revealed_square_string

fun concat_strings board =
  String.concat (
    Vector.foldr(fn(row,acc) =>
      Vector.foldr(fn(s,acc) => s::acc) ("\n"::acc) row) [] board )

val board_string = concat_strings o board_strings
val revealed_board_string = concat_strings o revealed_board_strings

fun press_square (c,Unpressed) = (c,Pressed)
  | press_square sq = sq

fun flag_square (c,Flagged) = (c,Unpressed)
  | flag_square (c,Unpressed) = (c,Flagged)
  | flag_square sq = sq

fun press c board =
  let
    val size = Vector.length board
    fun p ((ii,jj),board) =
      if is_Pressed (Vector.sub(Vector.sub(board,ii),jj))
      then board
      else
      let
        val board' =
          Vector.mapi(fn (i,row) =>
            Vector.mapi(fn (j,sq) =>
              if i = ii andalso j = jj then press_square sq else sq)
            row)
          board
      in
        if num_mines board' (ii,jj) = 0
        then
          List.foldl p board'
            (neighbour_coords size (ii,jj))
        else board'
      end
  in if on_board size c then p (c,board) else board end

print (board_string board)
print (revealed_board_string board)
val board2 = press (1,2) board
print (board_string board2)
val board3 = press (1,1) board2
print (board_string board3)
val board4 = press (1,0) board3
print (board_string board4)

val board2 = press (0,3) board
print (board_string board2)
val board3 = press (0,1) board2
print (board_string board3)

val () = TextIO.closeIn randomStream
