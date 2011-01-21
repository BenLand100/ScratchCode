#!/usr/bin/python

DAYS = {6:'Sunday',0:'Monday',1:'Tuesday',2:'Wednesday',3:'Thursday',4:'Friday',5:'Saturday'}
MONTHS = {1:'Jan',2:'Feb',3:'Mar',4:'Apr',5:'May',6:'Jun',7:'Jul',8:'Aug',9:'Sep',10:'Oct',11:'Nov',12:'Dec'}

year,month,day,idx = 1901,1,0,365

firstsuns = 0

while year < 2001:
    print DAYS[idx%7],', ',MONTHS[month],' ',day+1,', ',year
    if idx % 7 == 6 and day == 0:
        firstsuns += 1
    idx += 1
    day += 1
    if month == 1:
        if day == 31:
            day = 0
            month = 2
    elif month == 2:
        if (year % 4 == 0 and year % 100 != 0) or (year % 400 == 0 and year % 100 == 0):
            if day == 29:
                day = 0
                month = 3
        elif day == 28:
            day = 0
            month = 3
    elif month == 3:
        if day == 31:
            day = 0
            month = 4
    elif month == 4:
        if day == 30:
            day = 0
            month = 5
    elif month == 5:
        if day == 31:
            day = 0
            month = 6
    elif month == 6:
        if day == 30:
            day = 0
            month = 7
    elif month == 7:
        if day == 31:
            day = 0
            month = 8
    elif month == 8:
        if day == 31:
            day = 0
            month = 9
    elif month == 9:
        if day == 30:
            day = 0
            month = 10
    elif month == 10:
        if day == 31:
            day = 0
            month = 11
    elif month == 11:
        if day == 30:
            day = 0
            month = 12
    elif month == 12:
        if day == 31:
            day = 0
            month = 1
            year += 1

print year,month,day,idx
print firstsuns
