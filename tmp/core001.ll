; ModuleID = 'my init module'
source_filename = "my init module"

@0 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@1 = private unnamed_addr constant [2 x i8] c"=\00", align 1
@2 = private unnamed_addr constant [9 x i8] c"hello */\00", align 1
@3 = private unnamed_addr constant [9 x i8] c"/* world\00", align 1
@4 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1

declare void @printInt(i32 %0)

declare void @printString(i8* %0)

declare void @error()

declare i32 @readInt()

declare i8* @readString()

declare i1 @strs_eq(i8* %0, i8* %1)

declare i8* @merge_strs(i8* %0, i8* %1)

define i32 @main() {
start:
  %fncall = call i32 @fac(i32 10)
  call void @printInt(i32 %fncall)
  %fncall1 = call i32 @rfac(i32 10)
  call void @printInt(i32 %fncall1)
  %fncall2 = call i32 @mfac(i32 10)
  call void @printInt(i32 %fncall2)
  %fncall3 = call i32 @ifac(i32 10)
  call void @printInt(i32 %fncall3)
  br label %jmp_cond

jmp_body:                                         ; preds = %jmp_cond
  %0 = mul i32 %allocphi, %allocphi8
  %1 = add i32 %allocphi8, -1
  br label %jmp_cond

jmp_cond:                                         ; preds = %jmp_body, %start
  %allocphi8 = phi i32 [ 10, %start ], [ %1, %jmp_body ]
  %allocphi = phi i32 [ 1, %start ], [ %0, %jmp_body ]
  %2 = icmp sgt i32 %allocphi8, 0
  br i1 %2, label %jmp_body, label %jmp_after

jmp_after:                                        ; preds = %jmp_cond
  call void @printInt(i32 %allocphi)
  %fncall7 = call i8* @repStr(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @1, i32 0, i32 0), i32 60)
  call void @printString(i8* %fncall7)
  call void @printString(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @2, i32 0, i32 0))
  call void @printString(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @3, i32 0, i32 0))
  ret i32 0
}

define internal i32 @fac(i32 %a) {
start:
  br label %jmp_cond

jmp_body:                                         ; preds = %jmp_cond
  %0 = mul i32 %allocphi7, %allocphi
  %1 = sub i32 %allocphi, 1
  br label %jmp_cond

jmp_cond:                                         ; preds = %jmp_body, %start
  %allocphi7 = phi i32 [ 1, %start ], [ %0, %jmp_body ]
  %allocphi = phi i32 [ %a, %start ], [ %1, %jmp_body ]
  %2 = icmp sgt i32 %allocphi, 0
  br i1 %2, label %jmp_body, label %jmp_after

jmp_after:                                        ; preds = %jmp_cond
  ret i32 %allocphi7
}

define internal i32 @rfac(i32 %n) {
start:
  %0 = icmp eq i32 %n, 0
  br i1 %0, label %jmp_if, label %jmp_else

jmp_if:                                           ; preds = %start
  ret i32 1

jmp_else:                                         ; preds = %start
  %1 = sub i32 %n, 1
  %fncall = call i32 @rfac(i32 %1)
  %2 = mul i32 %n, %fncall
  ret i32 %2
}

define internal i32 @mfac(i32 %n) {
start:
  %0 = icmp eq i32 %n, 0
  br i1 %0, label %jmp_if, label %jmp_else

jmp_if:                                           ; preds = %start
  ret i32 1

jmp_else:                                         ; preds = %start
  %1 = sub i32 %n, 1
  %fncall = call i32 @nfac(i32 %1)
  %2 = mul i32 %n, %fncall
  ret i32 %2
}

define internal i32 @nfac(i32 %n) {
start:
  %0 = icmp ne i32 %n, 0
  br i1 %0, label %jmp_if, label %jmp_else

jmp_if:                                           ; preds = %start
  %1 = sub i32 %n, 1
  %fncall = call i32 @mfac(i32 %1)
  %2 = mul i32 %fncall, %n
  ret i32 %2

jmp_else:                                         ; preds = %start
  ret i32 1
}

define internal i32 @ifac(i32 %n) {
start:
  %fncall = call i32 @ifac2f(i32 1, i32 %n)
  ret i32 %fncall
}

define internal i32 @ifac2f(i32 %l, i32 %h) {
start:
  %0 = icmp eq i32 %l, %h
  br i1 %0, label %jmp_if, label %jmp_after

jmp_if:                                           ; preds = %start
  ret i32 %l

jmp_after:                                        ; preds = %start
  %1 = icmp sgt i32 %l, %h
  br i1 %1, label %jmp_if7, label %jmp_after8

jmp_if7:                                          ; preds = %jmp_after
  ret i32 1

jmp_after8:                                       ; preds = %jmp_after
  %2 = add i32 %l, %h
  %3 = sdiv i32 %2, 2
  %fncall = call i32 @ifac2f(i32 %l, i32 %3)
  %4 = add i32 %3, 1
  %fncall15 = call i32 @ifac2f(i32 %4, i32 %h)
  %5 = mul i32 %fncall, %fncall15
  ret i32 %5
}

define internal i8* @repStr(i8* %s, i32 %n) {
start:
  br label %jmp_cond

jmp_body:                                         ; preds = %jmp_cond
  %strconcat = call i8* @merge_strs(i8* %allocphi7, i8* %s)
  %0 = add i32 %allocphi, 1
  br label %jmp_cond

jmp_cond:                                         ; preds = %jmp_body, %start
  %allocphi7 = phi i8* [ getelementptr inbounds ([1 x i8], [1 x i8]* @4, i32 0, i32 0), %start ], [ %strconcat, %jmp_body ]
  %allocphi = phi i32 [ 0, %start ], [ %0, %jmp_body ]
  %1 = icmp slt i32 %allocphi, %n
  br i1 %1, label %jmp_body, label %jmp_after

jmp_after:                                        ; preds = %jmp_cond
  ret i8* %allocphi7
}
