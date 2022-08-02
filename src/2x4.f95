program three
    implicit none

    real :: x1, x2, y1, y2, z1, z2, eq1, eq2
    real, dimension(8) :: arr
    real :: xval, yval, zval

    call get_values(x1, x2, y1, y2, z1, z2, eq1, eq2)

    arr = get_as_arr(x1, x2, y1, y2, z1, z2, eq1, eq2)
    zval = get_zval(copy_arr(arr))
    yval = get_yval(copy_num(zval), copy_arr(arr))
    xval = get_xval(copy_num(zval), copy_num(yval), arr)

    print *, "X: ", xval, "Y: ", yval, "Z: ", zval

    contains 

    subroutine get_values(x1, x2, y1, y2, z1, z2, eq1, eq2)
        implicit none

        real :: x1, x2, y1, y2, z1, z2, eq1, eq2

        print *, "Enter An X1 Value: "
        read(*, *) x1

        print *, "Enter An Y1 Value: "
        read(*, *) y1

        print *, "Enter An Z1 Value: "
        read(*, *) z1

        print *, "Enter An EQ1 Value: "
        read(*, *) eq1

        print *, "Enter An X2 Value: "
        read(*, *) x2

        print *, "Enter An Y2 Value: "
        read(*, *) y2

        print *, "Enter An Z2 Value: "
        read(*, *) z2

        print *, "Enter An EQ2 Value: "
        read(*, *) eq2
    end subroutine get_values

    function get_as_arr(x1, x2, y1, y2, z1, z2, eq1, eq2) results(arr)
        implicit none

        real :: x1, x2, y1, y2, z1, z2, eq1, eq2
        real, dimension(8) :: arr

        arr(1) = real(x1)
        arr(2) = real(x2)
        arr(3) = real(y1)
        arr(4) = real(y2)
        arr(5) = real(z1)
        arr(6) = real(z2)
        arr(7) = real(eq1)
        arr(8) = real(eq2)

        return
    end function get_as_arr

    function copy_arr(val) result(copyval)
        implicit none

        real, dimension(8) :: val, copyval

        copyval = val

        return
    end function copy_arr

    function copy_num(val) result(copyval)
        implicit none

        real :: val, copyval

        copyval = val
        
        return
    end function copy_num

    function get_zval(arr) result(zval)
        implicit none
        
        real, dimension(8) :: arr
        real :: copy, ysum, zsum, eqsum
        real :: copy2, zsum2, eqsum2
        real :: zval

        copy = arr(1)

        arr(1) = arr(1) * arr(2)
        arr(3) = arr(3) * arr(2)
        arr(5) = arr(5) * arr(2)
        arr(7) = arr(7) * arr(2)

        arr(2) = arr(2) * copy * (-1)
        arr(4) = arr(4) * copy * (-1)
        arr(6) = arr(6) * copy * (-1)
        arr(8) = arr(8) * copy * (-1)

        ysum = arr(3) + arr(4)
        zsum = arr(5) + arr(6)
        eqsum = arr(7) + arr(8)

        copy2 = arr(3)

        arr(3) = arr(3) * arr(4)
        arr(5) = arr(5) * arr(4)
        arr(7) = arr(7) * arr(4)

        arr(4) = arr(4) * copy2 * (-1)
        arr(6) = arr(6) * copy2 * (-1)
        arr(8) = arr(8) * copy2 * (-1)

        zsum2 = arr(5) + arr(6)
        eqsum2 = arr(7) + arr(8)

        eqsum2 = eqsum2 / zsum2
        zval = eqsum2

        return
    end function get_zval

    function get_yval(zval, arr) result(yval)
        implicit none

        real, dimension(6) :: arr
        real :: copy, zval, ysum, eqsum, yval

        arr(5) = arr(5) * zval 
        arr(6) = arr(6) * zval

        arr(7) = arr(7) - arr(5)
        arr(8) = arr(8) - arr(6)

        copy = arr(1)

        arr(1) = arr(1) * arr(2)
        arr(3) = arr(3) * arr(2)
        arr(7) = arr(7) * arr(2)

        arr(2) = arr(2) * copy * (-1)
        arr(4) = arr(4) * copy * (-1)
        arr(8) = arr(8) * copy * (-1)

        ysum = arr(3) + arr(4)
        eqsum = arr(7) + arr(8)

        eqsum = eqsum / ysum
        yval = eqsum

        return
    end function get_yval

    function get_xval(zval, yval, arr) result(xval)
        implicit none

        real, dimension(8) :: arr
        real :: zval, yval, xval

        arr(3) = arr(3) * yval
        arr(4) = arr(4) * yval
        arr(5) = arr(5) * zval
        arr(6) = arr(6) * zval

        arr(7) = arr(7) - arr(5) - arr(3)
        arr(8) = arr(8) - arr(6) - arr(4)

        xval = arr(7) + arr(8)
        xval = xval / (arr(1) + arr(2))

        return
    end function get_xval
end program three