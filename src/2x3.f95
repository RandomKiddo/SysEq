program two
    implicit none

    real :: x1, x2, y1, y2, eq1, eq2
    real, dimension(6) :: arr
    real :: yval, xval

    call get_values(x1, x2, y1, y2, eq1, eq2)

    arr = get_as_arr(x1, x2, y1, y2, eq1, eq2)
    yval = get_yval(copy_arr(arr))
    xval = get_xval(copy_num(yval), arr)

    print *, "X: ", xval, "Y: ", yval

    contains

    subroutine get_values(x1, x2, y1, y2, eq1, eq2)
        implicit none

        real :: x1, x2, y1, y2, eq1, eq2

        print *, "Enter An X1 Value: "
        read(*, *) x1
        
        print *, "Enter An Y1 Value: "
        read(*, *) y1

        print *, "Enter An EQ1 Value: "
        read(*, *) eq1

        print *, "Enter An X2 Value: "
        read(*, *) x2

        print *, "Enter An Y2 Value: "
        read(*, *) y2

        print *, "Enter An EQ2 Value: "
        read(*, *) eq2
    end subroutine get_values

    function copy_arr(val) result(copyval)
        implicit none

        real, dimension(6) :: val, copyval

        copyval = val

        return
    end function copy_arr

    function copy_num(val) result(copyval)
        implicit none

        real :: val, copyval

        copyval = val
        
        return
    end function copy_num

    function get_as_arr(x1, x2, y1, y2, eq1, eq2) result(arr)
        implicit none

        real :: x1, x2, y1, y2, eq1, eq2
        real, dimension(6) :: arr

        arr(1) = real(x1)
        arr(2) = real(x2)
        arr(3) = real(y1)
        arr(4) = real(y2)
        arr(5) = real(eq1)
        arr(6) = real(eq2)

        return
    end function get_as_arr

    function get_yval(arr) result(yval)
        implicit none

        real, dimension(6) :: arr
        real :: yval, ysum, eqsum, copy

        copy = arr(1)

        arr(1) = arr(1) * arr(2)
        arr(3) = arr(3) * arr(2)
        arr(5) = arr(5) * arr(2)

        arr(2) = arr(2) * copy * (-1)
        arr(4) = arr(4) * copy * (-1)
        arr(6) = arr(6) * copy * (-1)

        ysum = arr(3) + arr(4)
        eqsum = arr(5) + arr(6)

        eqsum = eqsum / ysum
        yval = eqsum

        return
    end function get_yval

    function get_xval(yval, arr) result(xval)
        implicit none

        real :: yval, xval
        real, dimension(6) :: arr

        xval = arr(5) - (arr(3) * yval)
        xval = xval / arr(1)

        return
    end function get_xval
end program two