val randomStream = ref TextIO.stdIn

fun random_bit() =
  let val c = Option.valOf (TextIO.input1 (!randomStream))
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

fun board_sub board (i,j) =
  Vector.sub(Vector.sub(board,i),j)

fun square_string n (c,Flagged) = "!"
  | square_string n (c,Unpressed) = "+"
  | square_string n (Mine,Pressed) = "*"
  | square_string n (Safe,Pressed) = if n = 0 then " " else Int.toString n

fun revealed_square_string _ (Mine,Flagged) = "!"
  | revealed_square_string _ (_,Flagged) = "?"
  | revealed_square_string n (c,a) = square_string n (c,Pressed)

fun is_Mine (Mine,_) = true
  | is_Mine _ = false

fun is_Pressed (_,Pressed) = true
  | is_Pressed _ = false

fun is_Flagged (_,Flagged) = true
  | is_Flagged _ = false

fun neighbour_coords size (i,j) =
  List.filter (on_board size)
    [(i-1,j-1), (i,j-1), (i+1,j-1),
     (i-1,j  ),          (i+1,j  ),
     (i-1,j+1), (i,j+1), (i+1,j+1)]

fun num_mines board (i,j) =
  List.length
    (List.filter is_Mine
      (List.map (board_sub board)
       (neighbour_coords (Vector.length board) (i,j))))

fun mk_board_strings square_func board =
  Vector.mapi(fn(i,row) =>
    Vector.mapi(fn(j,sq) =>
      square_func (num_mines board (i,j)) sq)
    row)
  board

val board_strings = mk_board_strings square_string
val revealed_board_strings = mk_board_strings revealed_square_string

fun concat_strings_with_index board =
  String.concat (
    Vector.foldri(fn(i,row,acc) =>
      Vector.foldr(fn(s,acc) => s::acc)
        ("|"::Int.toString (i mod 10)::"\n"::acc) row)
      let val size = Vector.length board in
        List.tabulate(size,fn j => Int.toString (j mod 10))
      end
      board )

val board_string = concat_strings_with_index o board_strings
val revealed_board_string = concat_strings_with_index o revealed_board_strings

fun press_square (c,Unpressed) = (c,Pressed)
  | press_square sq = sq

fun flag_square (c,Flagged) = (c,Unpressed)
  | flag_square (c,Unpressed) = (c,Flagged)
  | flag_square sq = sq

fun press board c =
  let
    val size = Vector.length board
    fun p ((ii,jj),board) =
      if is_Pressed (board_sub board (ii,jj))
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

fun flag board (c as (ii,jj)) =
  if on_board (Vector.length board) c then
    Vector.mapi(fn (i,row) =>
      Vector.mapi(fn (j,sq) =>
        if i = ii andalso j = jj then flag_square sq else sq)
      row)
    board
  else board

fun auto_clear_aux board =
  let
    val size = Vector.length board
    fun looprow i (acc as (board,changed)) =
      if size <= i then acc else looprow (i+1)
      let
        fun loopcol j (acc as (board,changed)) =
          if size <= j then acc else loopcol (j+1)
            let
              val nbcs = neighbour_coords size (i,j)
              val nmines = List.length (List.filter (is_Mine o board_sub board) nbcs)
              val (fcs,ucs) = List.partition (is_Flagged o board_sub board) nbcs
            in
              if is_Pressed (board_sub board (i,j)) andalso
                 0 < nmines andalso nmines <= List.length fcs
              then
                (List.foldl (fn (c,b) => press b c) board ucs,
                 List.exists (not o is_Pressed o board_sub board) ucs)
              else acc
            end
      in loopcol 0 acc end
  in looprow 0 (board, false) end

fun auto_clear board =
  let val (board, changed) = auto_clear_aux board
  in if changed then auto_clear board else board end

val exploded =
  Vector.exists(fn row =>
    (Vector.exists (fn sq =>
      is_Pressed sq andalso is_Mine sq)
    row))

val cleared =
  not o
    Vector.exists(fn row =>
      (Vector.exists(fn sq =>
        is_Mine sq andalso
        not (is_Flagged sq))
      row))

fun prompt_num name default =
  let
    val () = print name
    val () = print " (default "
    val () = print (Int.toString default)
    val () = print ")?> "
    val optline = TextIO.inputLine TextIO.stdIn
  in
    case optline of SOME line =>
      (case Int.fromString line of SOME n =>
         if 0 <= n then n else default
       | _ => default)
    | _ => default
  end

fun new_game() =
  let
    val () = TextIO.output(TextIO.stdOut,"minesweeper\n")
    val size = prompt_num "Board size" 10
    val rate = prompt_num "Mine rate" 2
  in mk_board rate size end

datatype move = Press of int * int | Flag of int * int

fun usage() =
  let
    val () = print "<row> <col>   : press a square\n"
    val () = print "f <row> <col> : toggle flag on square\n"
  in () end

fun parse_move line =
  case String.tokens Char.isSpace line of
    ["f", row, col] =>
      Flag (Option.valOf (Int.fromString row),
            Option.valOf (Int.fromString col))
  | [row, col] =>
      Press (Option.valOf (Int.fromString row),
             Option.valOf (Int.fromString col))
  | _ => raise Option

fun main() =
  let
    val () = randomStream := TextIO.openIn"/dev/urandom"
    val board = new_game()
    val () = usage()
    fun play board =
      let
        val () = print (board_string board)
        val () = print "?> "
        val line = Option.valOf (TextIO.inputLine TextIO.stdIn)
        val board =
          case parse_move line of
            Press c => press board c
          | Flag c => auto_clear (flag board c)
      in
        if exploded board then
          (print "exploded!\n";
           print (revealed_board_string board);
           print "\n")
        else if cleared board then
          (print "cleared!\n";
           print (revealed_board_string board);
           print "\n")
        else play board
      end
      handle Option => (print"bad move\n"; usage(); play board)
    val () = play board
  in TextIO.closeIn (!randomStream) end
