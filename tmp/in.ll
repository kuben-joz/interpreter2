declare void @printInt(i32)

declare void @printString(i8*)

declare void @error()

declare i32 @readInt()

declare i8* @readString()

declare i1 @strs_eq(i8*, i8*)

declare i8* @merge_strs(i8*, i8*)

define internal i1 @id.1(i32 %r0) {
blk_0:
  call void @printInt(i32 %r0)
  %r1 = srem i32 %r0, 2
  %r2 = icmp eq i32 %r1, 1
  ret i1 %r2

}

define internal i1 @idb.1(i1 %r0) {
blk_0:
  br i1 %r0, label %blk_1, label %blk_2

blk_1:
  call void @printString(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @str_0, i32 0, i32 0))
  br label %blk_3

blk_2:
  call void @printString(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @str_1, i32 0, i32 0))
  br label %blk_3

blk_3:
  ret i1 %r0

}

define i32 @main() {
blk_0:
  %r0 = call i1 @id.1(i32 1)
  br i1 %r0, label %blk_1, label %blk_2

blk_1:
  call void @printString(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @str_2, i32 0, i32 0))
  ret i32 0

blk_2:
  %r1 = call i1 @id.1(i32 2)
  br i1 %r1, label %blk_1, label %blk_3

blk_3:
  ret i32 0

}

@str_2 = private constant [4 x i8] c"\61\62\73\00", align 1; "abs"
@str_1 = private constant [6 x i8] c"\66\61\6c\73\65\00", align 1; "false"
@str_0 = private constant [5 x i8] c"\74\72\75\65\00", align 1; "true"
