declare void @printInt(i32)

declare void @printString(i8*)

declare void @error()

declare i32 @readInt()

declare i8* @readString()

declare i1 @strs_eq(i8*, i8*)

declare i8* @merge_strs(i8*, i8*)

define i32 @main() {
blk_0:
  call void @f.1(i32 5, i32 3)
  call void @f.1(i32 -5, i32 3)
  ret i32 0

}

define internal void @f.1(i32 %r0, i32 %r1) {
blk_0:
  %r2 = srem i32 %r0, %r1
  call void @printInt(i32 %r2)
  ret void

}

