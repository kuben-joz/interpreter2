declare void @printInt(i32)

declare void @printString(i8*)

declare void @error()

declare i32 @readInt()

declare i8* @readString()

declare i1 @strs_eq(i8*, i8*)

declare i8* @merge_strs(i8*, i8*)

define internal i32 @f.1(i32 %r0) {
blk_0:
  br label %blk_1

blk_2:
  br label %blk_3

blk_4:
  %r1 = add i32 %r2, 100
  call void @printInt(i32 %r1)
  %r3 = add i32 %r4, 1
  %r5 = add i32 %r2, 1
  br label %blk_3

blk_3:
  %r4 = phi i32 [ %r6, %blk_2 ], [ %r3, %blk_4 ]
  %r2 = phi i32 [ %r0, %blk_2 ], [ %r5, %blk_4 ]
  %r7 = icmp slt i32 %r4, 2
  br i1 %r7, label %blk_4, label %blk_5

blk_5:
  br label %blk_1

blk_1:
  %r6 = phi i32 [ 0, %blk_0 ], [ %r4, %blk_5 ]
  %r8 = icmp slt i32 %r6, 1
  br i1 %r8, label %blk_2, label %blk_6

blk_6:
  ret i32 0

}

define i32 @main() {
blk_0:
  %r0 = call i32 @f.1(i32 12)
  ret i32 0

}

