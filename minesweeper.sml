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
    val empty_row = Vector.tabulate(size,(fn _ => empty_square))
  in (cols,empty_row) end

val mine_rate = 2
val size = 5
val (board,empty_row) = mk_board mine_rate size

fun square_string n (c,Flagged) = "!"
  | square_string n (c,Unpressed) = "+"
  | square_string n (Mine,Pressed) = "*"
  | square_string n (Safe,Pressed) = Int.toString n

fun revealed_square_string _ (Mine,_) = "*"
  | revealed_square_string _ (Safe,_) = "+"

fun is_Mine (Mine,_) = true
  | is_Mine _ = false

fun mk_row_strings square_func size prev row next =
  let
    fun safe_sub(v,i) = if i < 0 orelse size <= i then empty_square else Vector.sub(v,i)
    fun f (i,sq) =
      let
        val neighbours = [
          safe_sub(prev,i-1), safe_sub(prev,i), safe_sub(prev,i+1),
          safe_sub(row, i-1),                   safe_sub(row, i+1),
          safe_sub(next,i-1), safe_sub(next,i), safe_sub(next,i+1)]
        val mines = List.length (List.filter is_Mine neighbours)
      in square_func mines sq end
   in Vector.mapi f row end

val row_strings = mk_row_strings square_string
val revealed_row_strings = mk_row_strings revealed_square_string

val strs = revealed_row_strings size empty_row (Vector.sub(board,0)) (Vector.sub(board,1))

String.concat(List.tabulate(size,(fn i => Vector.sub(strs,i))))

fun print_board str
TextIO.output


val () = TextIO.closeIn randomStream
