declare void @printInt(i32)

declare void @printString(i8*)

declare void @error()

declare i32 @readInt()

declare i8* @readString()

declare i1 @strs_eq(i8*, i8*)

declare i8* @merge_strs(i8*, i8*)

define i32 @main() {
blk_0:
  %r0 = call i32 @f.1(i32 1, i32 -1)
  call void @printInt(i32 %r0)
  ret i32 0

}

define internal i32 @f.1(i32 %r0, i32 %r1) {
blk_0:
  %r2 = icmp sgt i32 %r0, 0
  br i1 %r2, label %blk_1, label %blk_2

blk_1:
  %r3 = icmp sgt i32 %r1, 0
  br i1 %r3, label %blk_3, label %blk_2

blk_2:
  %r4 = icmp slt i32 %r0, 0
  br i1 %r4, label %blk_4, label %blk_5

blk_4:
  %r5 = icmp slt i32 %r1, 0
  br i1 %r5, label %blk_3, label %blk_5

blk_3:
  ret i32 7

blk_5:
  ret i32 42

}

