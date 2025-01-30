declare void @printInt(i32)

declare void @printString(i8*)

declare void @error()

declare i32 @readInt()

declare i8* @readString()

declare i1 @strs_eq(i8*, i8*)

declare i8* @merge_strs(i8*, i8*)

define i32 @main() {
blk_0:
  %r0 = call i32 @readInt()
  %r1 = icmp slt i32 %r0, 0
  br i1 %r1, label %blk_1, label %blk_2

blk_1:
  ret i32 5

blk_2:
  ret i32 8

blk_3:

}

