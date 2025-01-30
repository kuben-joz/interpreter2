declare void @printInt(i32)

declare void @printString(i8*)

declare void @error()

declare i32 @readInt()

declare i8* @readString()

declare i1 @strs_eq(i8*, i8*)

declare i8* @merge_strs(i8*, i8*)

define i32 @main() {
blk_0:
  call void @printString(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str_0, i32 0, i32 0))
  call void @printString(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str_0, i32 0, i32 0))
  call void @printString(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str_1, i32 0, i32 0))
  call void @printString(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str_1, i32 0, i32 0))
  ret i32 0

}

@str_0 = private constant [2 x i8] c"\34\00", align 1; "4"
@str_1 = private constant [2 x i8] c"\36\00", align 1; "6"
