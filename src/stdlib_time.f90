module stdlib_time
    implicit none
    private

    public :: time_stamp


contains

function time_stamp()
!! Creates a time stamp in the format 'yyyy-mm-dd hh:mm:ss.sss'
    character(23) :: time_stamp
    character(8)  :: date
    character(10) :: time

    call date_and_time( date, time )

    time_stamp(1:4)   = date(1:4)
    time_stamp(5:5)   = '-'
    time_stamp(6:7)   = date(5:6)
    time_stamp(8:8)   = '-'
    time_stamp(9:10)  = date(7:8)
    time_stamp(11:11) = ' '
    time_stamp(12:13) = time(1:2)
    time_stamp(14:14) = ':'
    time_stamp(15:16) = time(3:4)
    time_stamp(17:17) = ':'
    time_stamp(18:23) = time(5:10)

end function time_stamp

end module stdlib_time
