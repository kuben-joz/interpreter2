declare void @printInt(i32)

declare void @printString(i8*)

declare void @error()

declare i32 @readInt()

declare i8* @readString()

declare i1 @strs_eq(i8*, i8*)

declare i8* @merge_strs(i8*, i8*)

define i32 @main() {
blk_0:
  ret i32 0

}

define internal i32 @f.1(i32 %r0) {
blk_0:
  br label %blk_1

blk_2:
  br label %blk_1

blk_1:
  %r1 = icmp ne i32 %r0, 0
  br i1 %r1, label %blk_2, label %blk_3

blk_3:
  ret i32 0

}

