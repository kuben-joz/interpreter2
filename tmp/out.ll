declare void @printInt(i32)

declare void @printString(i8*)

declare void @error()

declare i32 @readInt()

declare i8* @readString()

declare i1 @strs_eq(i8*, i8*)

declare i8* @merge_strs(i8*, i8*)

define i32 @main() {
blk_0:
  call void @printInt(i32 2)
  ret i32 0

}

