fun is_older(x: int*int*int, y: int*int*int) =
    if (#1 x) <> (#1 y)
    then (#1 x) < (#1 y)
    else if(#2 x) <> (#2 y)
    then (#2 x) < (#2 y)
    else if(#3 x) <> (#3 y)
    then (#3 x) < (#3 y)
    else false


fun number_in_month(xs: (int*int*int) list, month: int) =
    let fun helper(xs: (int*int*int) list, month: int, acc: int) =
	    if null xs
	    then acc
	    else helper(tl xs, month, acc + (if month = (#2 (hd xs)) then 1 else 0))
    in helper(xs, month, 0)
    end

fun number_in_months(xs: (int*int*int) list, month: int list) =
    let fun helper(month: int list, acc: int) =
	    if null month
	    then acc
	    else helper(tl month, acc + number_in_month(xs, hd month))
    in helper(month, 0)
    end

fun dates_in_month(xs: (int*int*int) list, month: int) =
    if null xs
    then []
    else if (#2 (hd xs)) <> month
    then dates_in_month(tl xs, month)
    else hd xs :: dates_in_month(tl xs, month)
				
fun dates_in_months(xs: (int*int*int) list, month: int list) =
    if null month
    then []
    else dates_in_month(xs, hd month) @ dates_in_months(xs, tl month)

fun get_nth(xs: string list, n: int) =
    if null xs
    then "invalid ans"
    else
	let fun helper(xs: string list, n: int, index: int) =
		if n=index
		then hd xs
		else helper(tl xs, n, index+1)
	in helper(xs, n, 1)
	end

fun date_to_string(date: int*int*int) =
    let val month_i2s = ["January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth(month_i2s, (#2 date))  ^ " " ^  Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date) 
    end

fun number_before_reaching_sum(sum: int, xs: int list) =
    let fun helper(sum: int, xs: int list, cnt: int, index: int) =
	    if(cnt + (hd xs) >= sum)
	    then index
	    else helper(sum, tl xs, cnt + (hd xs), index+1)
    in helper(sum, xs, 0, 0)
    end

fun what_month(day: int)  =
    let val month2days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum(day, month2days) + 1
    end
	
fun month_range(day1: int, day2: int) =
    let fun helper(day: int) =
	    if day>day2
	    then []
	    else what_month(day) :: helper(day+1)
    in helper(day1)
    end
	
fun oldest(xs: (int*int*int) list) =
    if null xs
    then NONE
    else
	let val candidate = oldest(tl xs)
	in if isSome candidate andalso is_older(valOf candidate, hd xs)
	   then candidate
	   else SOME(hd xs)
	end
		     	
fun remove_duplicates(months: int list) =
    let fun iscontains(target: int, ans: int list) =
	    if null ans
	    then false
	    else if (hd ans) = target
	    then true
	    else iscontains(target, tl ans)
    in if null months
       then []
       else let val candidate = remove_duplicates(tl months)
	    in if iscontains(hd months, candidate)
	       then candidate
	       else (hd months) :: candidate
	    end
    end
      
    
fun number_in_months_challenge(xs: (int*int*int) list, months: int list) =
    number_in_months(xs, remove_duplicates(months))
	
fun dates_in_months_challenge(xs: (int*int*int) list, months: int list) =
    dates_in_months(xs, remove_duplicates(months))
	
fun reasonable_date(data: int*int*int) =
    let val year = (#1 data)
	val month = (#2 data)
	val day = (#3 data)
    in
	if year < 1
	then false
	else if month <1 orelse month > 12
	then false
	else let val leap_month = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		 val unleap_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		 fun daysinmonth(months: int list, month: int, index: int) =
		     if month = index
		     then hd months
		     else daysinmonth(tl months, month, index+1)
	     in if (year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0))
		then if day < 1 orelse day > daysinmonth(leap_month, month, 1) then false else true
		else if day < 1 orelse day > daysinmonth(unleap_month, month, 1) then false else true
	     end
    end
		 
