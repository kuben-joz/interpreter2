declare void @printInt(i32)

declare void @printString(i8*)

declare void @error()

declare i32 @readInt()

declare i8* @readString()

declare i1 @strs_eq(i8*, i8*)

declare i8* @merge_strs(i8*, i8*)

define i32 @main() {
blk_0:
  %r0 = call i32 @fac.1(i32 10)
  call void @printInt(i32 %r0)
  %r1 = call i32 @rfac.1(i32 10)
  call void @printInt(i32 %r1)
  %r2 = call i32 @mfac.1(i32 10)
  call void @printInt(i32 %r2)
  %r3 = call i32 @ifac.1(i32 10)
  call void @printInt(i32 %r3)
  br label %blk_1

blk_2:
  %r4 = mul i32 %r5, %r6
  %r7 = add i32 %r6, -1
  br label %blk_1

blk_1:
  %r6 = phi i32 [ 10, %blk_0 ], [ %r7, %blk_2 ]
  %r5 = phi i32 [ 1, %blk_0 ], [ %r4, %blk_2 ]
  %r8 = icmp sgt i32 %r6, 0
  br i1 %r8, label %blk_2, label %blk_3

blk_3:
  call void @printInt(i32 %r5)
  %r9 = call i8* @repStr.1(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str_0, i32 0, i32 0), i32 60)
  call void @printString(i8* %r9)
  call void @printString(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @str_1, i32 0, i32 0))
  call void @printString(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @str_2, i32 0, i32 0))
  ret i32 0

}

define internal i32 @fac.1(i32 %r0) {
blk_0:
  br label %blk_1

blk_2:
  %r1 = mul i32 %r2, %r3
  %r4 = sub i32 %r3, 1
  br label %blk_1

blk_1:
  %r2 = phi i32 [ 1, %blk_0 ], [ %r1, %blk_2 ]
  %r3 = phi i32 [ %r0, %blk_0 ], [ %r4, %blk_2 ]
  %r5 = icmp sgt i32 %r3, 0
  br i1 %r5, label %blk_2, label %blk_3

blk_3:
  ret i32 %r2

}

define internal i32 @rfac.1(i32 %r0) {
blk_0:
  %r1 = icmp eq i32 %r0, 0
  br i1 %r1, label %blk_1, label %blk_2

blk_1:
  ret i32 1

blk_2:
  %r2 = sub i32 %r0, 1
  %r3 = call i32 @rfac.1(i32 %r2)
  %r4 = mul i32 %r0, %r3
  ret i32 %r4

}

define internal i32 @mfac.1(i32 %r0) {
blk_0:
  %r1 = icmp eq i32 %r0, 0
  br i1 %r1, label %blk_1, label %blk_2

blk_1:
  ret i32 1

blk_2:
  %r2 = sub i32 %r0, 1
  %r3 = call i32 @nfac.1(i32 %r2)
  %r4 = mul i32 %r0, %r3
  ret i32 %r4

}

define internal i32 @nfac.1(i32 %r0) {
blk_0:
  %r1 = icmp ne i32 %r0, 0
  br i1 %r1, label %blk_1, label %blk_2

blk_1:
  %r2 = sub i32 %r0, 1
  %r3 = call i32 @mfac.1(i32 %r2)
  %r4 = mul i32 %r3, %r0
  ret i32 %r4

blk_2:
  ret i32 1

}

define internal i32 @ifac.1(i32 %r0) {
blk_0:
  %r1 = call i32 @ifac2f.1(i32 1, i32 %r0)
  ret i32 %r1

}

define internal i32 @ifac2f.1(i32 %r0, i32 %r1) {
blk_0:
  %r2 = icmp eq i32 %r0, %r1
  br i1 %r2, label %blk_1, label %blk_2

blk_1:
  ret i32 %r0

blk_2:
  %r3 = icmp sgt i32 %r0, %r1
  br i1 %r3, label %blk_3, label %blk_4

blk_3:
  ret i32 1

blk_4:
  %r4 = add i32 %r0, %r1
  %r5 = sdiv i32 %r4, 2
  %r6 = call i32 @ifac2f.1(i32 %r0, i32 %r5)
  %r7 = add i32 %r5, 1
  %r8 = call i32 @ifac2f.1(i32 %r7, i32 %r1)
  %r9 = mul i32 %r6, %r8
  ret i32 %r9

}

define internal i8* @repStr.1(i8* %r0, i32 %r1) {
blk_0:
  br label %blk_1

blk_2:
  %r2 = call i8* @merge_strs(i8* %r3, i8* %r0)
  %r4 = add i32 %r5, 1
  br label %blk_1

blk_1:
  %r3 = phi i8* [ getelementptr inbounds ([1 x i8], [1 x i8]* @str_3, i32 0, i32 0), %blk_0 ], [ %r2, %blk_2 ]
  %r5 = phi i32 [ 0, %blk_0 ], [ %r4, %blk_2 ]
  %r6 = icmp slt i32 %r5, %r1
  br i1 %r6, label %blk_2, label %blk_3

blk_3:
  ret i8* %r3

}

@str_3 = private constant [1 x i8] zeroinitializer, align 1; empty string
@str_2 = private constant [9 x i8] c"\2f\2a\20\77\6f\72\6c\64\00", align 1; "/* world"
@str_0 = private constant [2 x i8] c"\3d\00", align 1; "="
@str_1 = private constant [9 x i8] c"\68\65\6c\6c\6f\20\2a\2f\00", align 1; "hello */"
