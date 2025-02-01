define internal i32 @f(i32 %i) {
start:
  %0 = alloca i32, align 4
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %i1 = alloca i32, align 4
  store i32 %i, i32* %i1, align 4
  store i32 0, i32* %2, align 4
  br label %jmp_cond7

jmp_body:                                         ; preds = %jmp_cond7
  %loadvar = load i32, i32* %i1, align 4
  store i32 %loadvar, i32* %1, align 4
  br label %jmp_cond

jmp_body2:                                        ; preds = %jmp_cond
  %loadvar3 = load i32, i32* %1, align 4
  %3 = add i32 %loadvar3, 100
  store i32 %3, i32* %0, align 4
  %loadvar4 = load i32, i32* %0, align 4
  call void @printInt(i32 %loadvar4)
  %to_inc = load i32, i32* %2, align 4
  %4 = add i32 %to_inc, 1
  store i32 %4, i32* %2, align 4
  %to_inc5 = load i32, i32* %1, align 4
  %5 = add i32 %to_inc5, 1
  store i32 %5, i32* %1, align 4
  br label %jmp_cond

jmp_cond:                                         ; preds = %jmp_body2, %jmp_body
  %loadvar6 = load i32, i32* %2, align 4
  %6 = icmp slt i32 %loadvar6, 2
  br i1 %6, label %jmp_body2, label %jmp_after

jmp_after:                                        ; preds = %jmp_cond
  br label %jmp_cond7

jmp_cond7:                                        ; preds = %jmp_after, %start
  %loadvar8 = load i32, i32* %2, align 4
  %7 = icmp slt i32 %loadvar8, 1
  br i1 %7, label %jmp_body, label %jmp_after9

jmp_after9:                                       ; preds = %jmp_cond7
  ret i32 0
}

define i32 @main() {
start:
  %0 = alloca i32, align 4
  %fncall = call i32 @f(i32 12)
  store i32 %fncall, i32* %0, align 4
  ret i32 0
}