%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, %struct._IO_codecvt*, %struct._IO_wide_data*, %struct._IO_FILE*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type opaque
%struct._IO_codecvt = type opaque
%struct._IO_wide_data = type opaque

@d_read = internal constant [3 x i8] c"%d\00"
@d_write = internal constant [4 x i8] c"%d\0A\00"
;
@d = internal constant [3 x i8] c"%d\00"
@malloc_fail_conc = internal constant [29 x i8] c"malloc fail on string concat\00"
@read_str_fail = internal constant [28 x i8] c"getline failed on getString\00"
@run_err = internal constant [14 x i8] c"runtime error\00"
;@lf  = internal constant [4 x i8] c"%lf\00"	
; --------------  all of these can produce name conflicts I think --------------
@stdin = external global %struct._IO_FILE*
; make sure this doesn't conflict
; just add garbage to name until doesn't conflict maybe
declare dso_local i32 @printf(i8*, ...) 
declare dso_local i32 @scanf(i8*, ...)
declare dso_local i32 @puts(i8*)
declare dso_local i8* @malloc(i64)
declare dso_local void @free(i8*)
declare dso_local i64 @strlen(i8*)
declare dso_local i8* @strcpy(i8*, i8*)
declare dso_local i8* @strcat(i8*, i8*)
declare dso_local i32 @strcmp(i8*, i8*)
declare dso_local void @exit(i32)
declare dso_local i64 @getline(i8**, i64*, %struct._IO_FILE*)
declare dso_local i32 @getc(%struct._IO_FILE*)

define i1 @strs_eq(i8* %s1, i8* %s2) {
	entry:
		%i0 = call i32 @strcmp(i8* %s1, i8* %s2)
		%i1 = icmp eq i32 %i0, 0
		ret i1 %i1
}

; this is taken from lecture notes
define i8* @merge_strs(i8* %s1, i8* %s2) {
	entry:
		%i1 = call i64 @strlen(i8* %s1)
		%i2 = call i64 @strlen(i8* %s2)
		%i3 = add i64 %i1, 1
		%i4 = add i64 %i3, %i2
		%i5 = call i8* @malloc(i64 %i4); todo add nullptr check
		%i6 = icmp eq i8* %i5, null
		br i1 %i6, label %fail, label %success
	success:
		%i7 = call i8* @strcpy(i8* %i5, i8* %s1)
		%i8 = call i8* @strcat(i8* %i5, i8* %s2)
		ret i8* %i5
	fail:
		%i9 = getelementptr [29 x i8], [29 x i8]* @malloc_fail_conc, i32 0, i32 0
		%i10 = call i32 @puts(i8* %i9)
		call void @exit(i32 -1)
		ret i8* null
}

; --------------  all of these can produce name conflicts I think --------------

define void @printInt(i32 %x) {
	entry:
		%t0 = getelementptr [4 x i8], [4 x i8]* @d_write, i32 0, i32 0
		call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
		ret void
}

define void @printString(i8* %s) {
	entry:  
		call i32 @puts(i8* %s)
		ret void
}

define i8* @readString() {
	entry:
		%res_ref = alloca i8*
		%num_malloc = alloca i64
		store i8* null, i8** %res_ref
		store i64 0, i64* %num_malloc
		%stream = load %struct._IO_FILE*, %struct._IO_FILE** @stdin
		%num_read = call i64 @getline(i8** %res_ref, i64* %num_malloc, %struct._IO_FILE* %stream)
		%read_succ = icmp sge i64 %num_read, 0
		%res = load i8*, i8** %res_ref
		br i1 %read_succ, label %success, label %fail
	success: 
		%last_c_pos = sub i64 %num_read, 1
		%empty_edge = icmp slt i64 %last_c_pos, 0
		br i1 %empty_edge, label %succ_end, label %check_nl
	check_nl:
		%last_c_ptr = getelementptr i8, i8* %res, i64 %last_c_pos
		%last_c = load i8, i8* %last_c_ptr
		%is_nl = icmp eq i8 %last_c, 10; hex is reserved for floats in llvm
		br i1 %is_nl, label %rem_nl, label %succ_end
	rem_nl:
		store i8 0, i8* %last_c_ptr
		br label %succ_end
	succ_end:
		ret  i8* %res
	fail:
		call void @free(i8* %res)
		%err_msg = getelementptr [28 x i8], [28 x i8]* @read_str_fail, i32 0, i32 0
		%puts_tmp = call i32 @puts(i8* %err_msg)
		call void @exit(i32 -1)
		ret i8* null
}

define i32 @readInt() {
entry:
	%res = alloca i32
  %t1 = getelementptr [3 x i8], [3 x i8]* @d_read, i32 0, i32 0
	call i32 (i8*, ...) @scanf(i8* %t1, i32* %res)
	%stream = load %struct._IO_FILE*, %struct._IO_FILE** @stdin
	%t2 = call i32 @getc(%struct._IO_FILE* %stream)
	%t3 = load i32, i32* %res
	ret i32 %t3
}

define void @error() {
	entry:
		%i0 = getelementptr [14 x i8], [14 x i8]* @run_err, i32 0, i32 0
		call i32 @puts(i8* %i0)
		call void @exit(i32 1)
		ret void
}


;define i32 @main() {
;		%i1 = call i32 @readInt()
;		call void @printInt(i32 %i1)
;		%i2= getelementptr [28 x i8], [28 x i8]* @read_str_fail, i32 0, i32 0
;		%i3 = getelementptr [29 x i8], [29 x i8]* @malloc_fail_conc, i32 0, i32 0
;		%i4 = getelementptr [14 x i8], [14 x i8]* @run_err, i32 0, i32 0
;		%iext = call i8* @readString()
;		%i5 = call i1 @strs_eq(i8* %i2, i8* %i2)
;		br i1 %i5, label %is_true, label %is_false
;	is_true:
;		call void @printString(i8* %iext)
;		ret i32 0
;	is_false:
;		call void @printString(i8* %i3)
;		ret i32 0
;}
