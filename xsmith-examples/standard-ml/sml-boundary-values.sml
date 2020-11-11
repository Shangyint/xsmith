
fun main() = let

val max = case Int.minInt of
  NONE => 0
  | SOME x => x
val min = case Int.maxInt of
  NONE => 0
  | SOME x => x


val _ = print "Int.minInt: "
val _ = print(Int.toString min)
val _ = print "\n"
val _ = print "Int.maxInt: "
val _ = print(Int.toString max)
val _ = print "\n"

val _ = print "Char.minChar: "
val _ = print(Int.toString (Char.ord Char.minChar))
val _ = print "\n"
val _ = print "Char.maxChar: "
val _ = print(Int.toString (Char.ord Char.maxChar))
val _ = print "\n"
val _ = print "Char.maxOrd: "
val _ = print(Int.toString Char.maxOrd)
val _ = print "\n"

in print "\n" end

val _ = main()

