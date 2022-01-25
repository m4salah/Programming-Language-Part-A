fun is_older((year1: int, month1: int, day1: int), (year2: int, month2: int, day2: int)) = 
    if year1 < year2
    then true
    else if (year1 = year2) andalso (month1 < month2)
    then true
    else if (year1 = year2) andalso (month1 = month2) andalso (day1 < day2)
    then true
    else false

fun date_to_string(day: int, month: int, year: int) = 
    let fun get_nth(ss: string list, n:int) = 
        if n = 1
        then hd ss
        else get_nth(tl ss, n - 1)
    val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, month) ^ " " ^ Int.toString(year) ^ ", " ^ Int.toString(day)
    end

fun get_nth(ss: string list, n:int) = 
    if n = 1
    then hd ss
    else get_nth(tl ss, n - 1)

fun number_before_reaching_sum(sum: int, ll: int list) = 
    if hd(ll) < sum
    then 1 + number_before_reaching_sum(sum - hd ll, tl ll)
    else 0


fun number_in_month (dates: (int*int*int) list, month: int) = 
    if null dates
    then 0
    else if (null (tl dates)) andalso #2 (hd(dates)) = month
    then 1  
    else
    let val ans_tl = number_in_month(tl dates, month)
    in
        if #2 (hd(dates)) = month
        then 1 + ans_tl
        else ans_tl
    end

fun number_in_months(dates: (int*int*int) list, months: int list) = 
    if null months
    then 0
    else 
    let fun number_in_month (dates: (int*int*int) list, month: int) = 
        if null dates
        then 0
        else if (null (tl dates)) andalso #2 (hd(dates)) = month
        then 1  
        else
        let val ans_tl = number_in_month(tl dates, month)
        in
            if #2 (hd(dates)) = month
            then 1 + ans_tl
            else ans_tl
        end
    in
        number_in_month(dates, hd months) + number_in_months(dates, tl months)
    end
fun dates_in_months(dates: (int*int*int) list, months: int list) = 
    if null months
    then []
    else
    let fun dates_in_month(dates: (int*int*int) list, month: int) = 
        if null dates
        then []
        else 
        let val ans_tl = dates_in_month(tl dates, month)
        in
            if #2 (hd(dates)) = month
            then hd(dates)::ans_tl
            else ans_tl
        end
    in
        dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
    end

fun dates_in_month(dates: (int*int*int) list, month: int) = 
    if null dates
    then []
    else 
    let val ans_tl = dates_in_month(tl dates, month)
    in
        if #2 (hd(dates)) = month
        then hd(dates)::ans_tl
        else ans_tl
    end

fun what_month(day: int) =
    if day = 60
    then 3
    else if day <= 31
    then 1
    else if day > 330
    then 12
    else if day mod 30 > 0
    then day div 30 + 1
    else day div 30

fun month_range(day1: int, day2: int) = 
    if day1 > day2
    then []
    else what_month(day1):: month_range(day1 + 1, day2)

fun oldest(dates: (int*int*int) list) = 
    if null dates
    then NONE
    else 
    let val ans_temp = oldest(tl dates)
    in
        if isSome ans_temp andalso is_older(hd dates, valOf ans_temp)
        then ans_temp
        else SOME(hd dates)
    end