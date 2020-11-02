module constants
implicit none

	character(len=15) :: CARD_NAME_LIST = "3456789TJQKA2BR"
	integer,dimension(15) :: CARD_VALUE_LIST = (/ 1,2,3,4,5,6,7,8,9,10,11,12,14,16,18 /)
	integer,dimension(18) :: EIGHTEEN_ONES = (/ 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 /)
	integer,dimension(18,4) :: SCALE_MATRIX = RESHAPE(&
[1000000,2000000,3000000,4000000,5000000,6000000,7000000,9000000,9000000,10000000,11000000,12000000,0,14000000,0,16000000,0,18000000,&
 10000,  20000,  30000,  40000,  50000,  60000,  70000,  80000,  90000,  100000,  110000,  120000,  0,140000,  0,160000,  0,180000,&
 100,	 200,	 300,	 400,	 500,	 600,	 700,	 800,	 900,	 1000,	  1100,	   1200,	0,1400,	   0,1600,	  0,1800,&
 1,	     2,	   	 3,	   	 4,	     5,	     6,	     7,		 8,		 9,		 10,	  11,	   12,		0,14,	   0,16,	  0,18],[18,4])
	type C
	integer,dimension(20) :: values
	character(len=20) :: names
	end type C

	type M
		! 3 4 5 6 7 8 9 10J Q K A   2   B   R
		! 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		! 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0
		! 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 
		! 0 1 1 0 1 1 1 0 0 1 0 0 0 0 0 0 0 0 
		! matrix above is cards "77788899945Q"
		integer, dimension(4,18) :: matrix 
	end type M

	type P
		! 3 4 5 6 7 8 9 10J Q K A   2   B   R
		! 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  <--num of '1' as elements(4)
		! 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0  <--num of '1' as elements(3) ,num of consecutive '1' as elements(5)
		! 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0  <--num of '1' as elements(2)
		! 0 1 1 0 1 1 1 0 0 1 0 0 0 0 0 0 0 0  <--num of '1' as elements(1)
		! matrix above is cards "77788899945Q", pattern of it is (/6,3,3,0,3/), elements(5) is the num of consecutive key cards,
		! the value of cards depends on key cards, cards "777888999" are key cards of cards
		integer,dimension(5) :: elements 
	end type P
	type(P) :: p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,&
			   p19,p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,p31,p32,p33,p34,p35,p36
	type(P), dimension(36) :: PATTERN_DB

	common CARD_NAME_LIST
	common CARD_VALUE_LIST

contains
	subroutine init_pattern()
		p1%elements = (/ 1,0,0,0,1/)   ! single
		p2%elements = (/ 5,0,0,0,5/)   ! single x 5
		p3%elements = (/ 6,0,0,0,6/)   ! single x 6
		p4%elements = (/ 7,0,0,0,7/)   ! single x 7
		p5%elements = (/ 8,0,0,0,8/)   ! single x 8
		p6%elements = (/ 9,0,0,0,9/)   ! single x 9
		p7%elements = (/ 10,0,0,0,10/) ! single x 10
		p8%elements = (/ 11,0,0,0,11/) ! single x 11
		p9%elements = (/ 12,0,0,0,12/) ! single x 12
		p10%elements = (/ 1,1,0,0,1/)    ! pair
		p11%elements = (/ 3,3,0,0,3/)    ! pair x 3
		p12%elements = (/ 4,4,0,0,4/)    ! pair x 4
		p13%elements = (/ 5,5,0,0,5/)    ! pair x 5
		p14%elements = (/ 6,6,0,0,6/)    ! pair x 6
		p15%elements = (/ 7,7,0,0,7/)    ! pair x 7
		p16%elements = (/ 8,8,0,0,8/)    ! pair x 8
		p17%elements = (/ 9,9,0,0,9/)    ! pair x 9
		p18%elements = (/ 10,10,0,0,10/) ! pair x 10
		p19%elements = (/ 1,1,1,0,1/)     ! triple
		p20%elements = (/ 2,1,1,0,1/)     ! triple + single
		p21%elements = (/ 2,2,1,0,1/)     ! triple + pair
		p22%elements = (/ 2,2,2,0,2/)     ! triple x 2
		p23%elements = (/ 4,2,2,0,2/)     ! triple x 2 + single x 2
		p24%elements = (/ 4,4,2,0,2/)     ! triple x 2 + pair x 2
		p25%elements = (/ 3,3,3,0,3/)     ! triple x 3
		p26%elements = (/ 6,3,3,0,3/)     ! triple x 3 + single x 3
		p27%elements = (/ 6,6,3,0,3/)     ! triple x 3 + pair x 3
		p28%elements = (/ 4,4,4,0,4/)     ! triple x 4
		p29%elements = (/ 8,4,4,0,4/)     ! triple x 4 + single x 4
		p30%elements = (/ 8,8,4,0,4/)     ! triple x 4 + pair x 4
		p31%elements = (/ 5,5,5,0,5/)     ! triple x 5
		p32%elements = (/ 10,5,5,0,5/)    ! triple x 5 + single x 5
		p33%elements = (/ 6,6,6,0,6/)     ! triple x 6
		p34%elements = (/ 1,1,1,1,1/) 	 	! bomb
		p35%elements = (/ 3,1,1,1,1/) 	 	! bomb + single + single
		p36%elements = (/ 3,3,1,1,1/) 	 	! bomb + pair + pair
		PATTERN_DB = (/p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,&
					   p19,p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,p31,p32,p33,p34,p35,p36/)
	end subroutine init_pattern

	function new_matrix()
		type(M) :: new_matrix
		new_matrix%matrix = RESHAPE([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
									 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
									 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
									 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[4,18])
	end function new_matrix	

	function cards_to_matrix(cards)
		type(C) :: cards
		type(M) :: cards_to_matrix
		integer :: i,value
		cards_to_matrix = new_matrix()
		
		do i = 1, 20
			value = cards%values(i)
			if(value /= 0) then
				if(cards_to_matrix%matrix(1,value) == 0) then
					cards_to_matrix%matrix(1,value) = 1
				else if(cards_to_matrix%matrix(2,value) == 0) then
					cards_to_matrix%matrix(2,value) = 1
				else if(cards_to_matrix%matrix(3,value) == 0) then
					cards_to_matrix%matrix(3,value) = 1
				else if(cards_to_matrix%matrix(4,value) == 0) then
					cards_to_matrix%matrix(4,value) = 1
				end if
			end if
		end do
		
	end function cards_to_matrix

	function all_ones_consecutive(array)
		integer, dimension(18) :: array
		logical :: all_ones_consecutive
		integer :: first_one_index, last_one_index, i, sum_result

		i = 1
		do while(i <= 18)
			if(array(i) == 1) then
				first_one_index = i
				exit
			end if
			i = i + 1
		end do

		i = 18
		do while(i >= 1)
			if(array(i) == 1) then
				last_one_index = i
				exit
			end if
			i = i - 1
		end do

		sum_result = 0
		do i = first_one_index, last_one_index
			sum_result = sum_result + array(i)
		end do

		if(last_one_index - first_one_index + 1 == sum_result) then
			all_ones_consecutive = .true.
		else
			all_ones_consecutive = .false.
		end if						
	end function all_ones_consecutive

	function get_pattern(matrix)
		type(M) :: matrix
		type(P) :: get_pattern

		get_pattern%elements(1) = dot_product(matrix%matrix(1,:),EIGHTEEN_ONES)
		get_pattern%elements(2) = dot_product(matrix%matrix(2,:),EIGHTEEN_ONES)
		get_pattern%elements(3) = dot_product(matrix%matrix(3,:),EIGHTEEN_ONES)
		get_pattern%elements(4) = dot_product(matrix%matrix(4,:),EIGHTEEN_ONES)
		get_pattern%elements(5) = 0
		
		if(get_pattern%elements(4) /= 0) then
			if(all_ones_consecutive(matrix%matrix(4,:))) then
				get_pattern%elements(5) = get_pattern%elements(4)
			end if
		else if(get_pattern%elements(3) /= 0) then
			if(all_ones_consecutive(matrix%matrix(3,:))) then
				get_pattern%elements(5) = get_pattern%elements(3)
			end if
		else if(get_pattern%elements(2) /= 0) then
			if(all_ones_consecutive(matrix%matrix(2,:))) then
				get_pattern%elements(5) = get_pattern%elements(2)
			end if
		else if(get_pattern%elements(1) /= 0) then
			if(all_ones_consecutive(matrix%matrix(1,:))) then
				get_pattern%elements(5) = get_pattern%elements(1)
			end if
		end if

	end function get_pattern

	function equal_pattern(pattern, another_pattern)
		type(P) :: pattern, another_pattern
		logical :: equal_pattern
		equal_pattern = .false.

		if(pattern%elements(1) == another_pattern%elements(1).and.&
		   pattern%elements(2) == another_pattern%elements(2).and.&
		   pattern%elements(3) == another_pattern%elements(3).and.&
		   pattern%elements(4) == another_pattern%elements(4).and.&
		   pattern%elements(5) == another_pattern%elements(5)) then
			equal_pattern = .true.
		end if
	end function equal_pattern

	function get_pattern_id(pattern)
		type(P) :: pattern
		integer :: get_pattern_id, i
		get_pattern_id = 0
		do i = 1, 36	
			if(equal_pattern(pattern,PATTERN_DB(i))) then
				get_pattern_id = i
				exit
			end if
		end do
	end function get_pattern_id

	function is_nuke(cards)
		type(C) :: cards
		logical :: is_nuke

		call sort(cards)
		if(cards%values(1) == 16 .and. cards%values(2) == 18 .and. cards%values(3) == 0) then
			is_nuke = .true.
		else
			is_nuke = .false.
		end if
	end function is_nuke

	function has_pattern(cards)
		type(C) :: cards
		type(M) :: matrix
		type(P) :: pattern
		integer :: pattern_id
		logical :: has_pattern
		
		matrix = cards_to_matrix(cards)
		pattern = get_pattern(matrix)
		pattern_id = get_pattern_id(pattern)

		if(pattern_id == 0) then
			has_pattern = .false.
		else
			has_pattern = .true.
		end if
	
		if(is_nuke(cards)) then
			has_pattern = .true.
		end if

	end function has_pattern

	function same_pattern(cards, another_cards)
		type(C) :: cards, another_cards
		type(M) :: matrix_1, matrix_2
		type(P) :: pattern_1, pattern_2
		logical :: same_pattern
		
		matrix_1 = cards_to_matrix(cards)
		pattern_1 = get_pattern(matrix_1)

		matrix_2 = cards_to_matrix(another_cards)
		pattern_2 = get_pattern(matrix_2)

		if(get_pattern_id(pattern_1) == get_pattern_id(pattern_2)) then
			same_pattern = .true.
		else
			same_pattern = .false.
		end if
	end function same_pattern

	function pattern_latter_bigger(cards, another_cards)
		type(C) :: cards, another_cards
		type(M) :: matrix_1, matrix_2
		type(P) :: pattern_1, pattern_2
		integer :: pattern_id_1, pattern_id_2
		logical :: pattern_latter_bigger
		pattern_latter_bigger = .false.
		
		matrix_1 = cards_to_matrix(cards)
		pattern_1 = get_pattern(matrix_1)
		pattern_id_1 = get_pattern_id(pattern_1)

		matrix_2 = cards_to_matrix(another_cards)
		pattern_2 = get_pattern(matrix_2)
		pattern_id_2 = get_pattern_id(pattern_2)

		call sort(another_cards)
		! latter is nuke, another is not
		if(is_nuke(another_cards) .and. pattern_id_1 /= 0) then
			pattern_latter_bigger = .true.
		end if
	
		! latter is bomb, another is not nuke and bomb
		if(pattern_id_2 == 34 .and. pattern_id_1 /= 0 .and. pattern_id_1 /= 34) then
			pattern_latter_bigger = .true.
		end if
		
	end function pattern_latter_bigger

	function latter_bigger(cards,another_cards)
		type(C) :: cards, another_cards
		type(M) :: matrix_1, matrix_2
		integer :: cards_value, another_cards_value, tmp_value_1,tmp_value_2,tmp_value_3,tmp_value_4
		logical :: latter_bigger

		matrix_1 = cards_to_matrix(cards)
		matrix_2 = cards_to_matrix(another_cards)

		tmp_value_1 = dot_product(matrix_1%matrix(4,:), SCALE_MATRIX(:,1))
		tmp_value_2 = dot_product(matrix_1%matrix(3,:), SCALE_MATRIX(:,2))
		tmp_value_3 = dot_product(matrix_1%matrix(2,:), SCALE_MATRIX(:,3))
		tmp_value_4 = dot_product(matrix_1%matrix(1,:), SCALE_MATRIX(:,4))
		if(tmp_value_1 /= 0) then 
			cards_value = tmp_value_1
		else if(tmp_value_2 /= 0) then 
			cards_value = tmp_value_2
		else if(tmp_value_3 /= 0) then 
			cards_value = tmp_value_3
		else if(tmp_value_4 /= 0) then 
			cards_value = tmp_value_4
		end if
			
		tmp_value_1 = dot_product(matrix_2%matrix(4,:), SCALE_MATRIX(:,1))
		tmp_value_2 = dot_product(matrix_2%matrix(3,:), SCALE_MATRIX(:,2))
		tmp_value_3 = dot_product(matrix_2%matrix(2,:), SCALE_MATRIX(:,3))
		tmp_value_4 = dot_product(matrix_2%matrix(1,:), SCALE_MATRIX(:,4))
		if(tmp_value_1 /= 0) then 
			another_cards_value = tmp_value_1
		else if(tmp_value_2 /= 0) then 
			another_cards_value = tmp_value_2
		else if(tmp_value_3 /= 0) then 
			another_cards_value = tmp_value_3
		else if(tmp_value_4 /= 0) then 
			another_cards_value = tmp_value_4
		end if

		if(cards_value < another_cards_value) then
			latter_bigger = .true.
		else
			latter_bigger = .false.			
		end if
	end function latter_bigger

	function new_cards()
		type(C) :: new_cards
		new_cards%values = (/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
		new_cards%names = "                    "
	end function new_cards

	function gen_random()
		integer :: gen_random
		integer,dimension(8) :: values
		
		gen_random = 0
		do while(gen_random == 0)
			call date_and_time(VALUES=values)
			gen_random = values(8) * values(7) * values(6) * values(5)
		end do
			 			
	end function gen_random

	function shuffled_deck()
		integer :: i,card,random,seed
		integer,dimension(54) :: deck, shuffled_deck
		deck = (/ 1,2,3,4,5,6,7,8,9,10,11,12,14,&
				  1,2,3,4,5,6,7,8,9,10,11,12,14,&
				  1,2,3,4,5,6,7,8,9,10,11,12,14,&
				  1,2,3,4,5,6,7,8,9,10,11,12,14,&
				  16,18 /)
		i = 1
		seed = gen_random()

		do while(MAXVAL(deck) /= 0)
			call srand(seed)
			seed = seed + 1
			random = rand()*100000000
			random = mod(random,55)
			if(random == 0 .or. deck(random) == 0) then			
				cycle			
			end if
			!print*, random
			shuffled_deck(i) = deck(random)
			!print*, shuffled_deck(i)
			deck(random) = 0
			i = i + 1
		end do
		
	end function shuffled_deck

	function is_empty(cards)
		logical :: is_empty
		type(C) :: cards

		is_empty = .false.

		if(MAXVAL(cards%values) == 0) then
		is_empty = .true.
		end if

	end function is_empty

	function exists(cards, sub_cards)
		type(C) :: cards, sub_cards, cards_copy
		integer :: i,j
		logical :: exists
		exists = .true.
		cards_copy = new_cards()
		cards_copy%values(1:20) = cards%values(1:20)

		iloop: do i = 1, 20
			if(sub_cards%values(i) == 0) then
				cycle
			end if

			jloop: do j = 1, 20
				if(cards_copy%values(j) == sub_cards%values(i)) then
					call remove_single(cards_copy, cards_copy%values(j))
					exit jloop
				end if

				if(j == 20) then
					exists = .false.
					exit iloop
				end if
			end do jloop
		end do iloop

	end function exists
end module constants

program test
use constants
implicit none
	integer :: counts,pass_counts
	integer,dimension(54) :: deck
	type(C) :: john, mary, lord, current_player, played_cards, last_played_cards

	counts = 3
	pass_counts = 0
	john = new_cards()
	mary = new_cards()
	lord = new_cards()
	last_played_cards = new_cards()

	call init_pattern()
	deck = shuffled_deck()
	
	john%values(1:17) = deck(1:17)
	mary%values(1:17) = deck(18:34)
	lord%values(1:20) = deck(35:54)

	call sort(john)
	call sort(mary)
	call sort(lord)

	do while(.true.)
		if(mod(counts,3) == 0) then
			current_player%values(1:20) = lord%values(1:20)
		else if(mod(counts,3) == 1) then
			current_player%values(1:20) = john%values(1:20)
		else if(mod(counts,3) == 2) then
			current_player%values(1:20) = mary%values(1:20)
		end if

		call show(current_player)
		print*, "Play:"
		played_cards = new_cards()
		call wait_input(played_cards)

		if(is_empty(played_cards) .and. pass_counts < 2) then
			print*, "PASS"
			counts = counts + 1
			pass_counts = pass_counts + 1
			cycle
		else if(is_empty(played_cards) .and. pass_counts == 2) then
			print*, "Can't pass, finish your job!"
			cycle
		else if(pass_counts == 2) then
			last_played_cards = new_cards()
		end if
		pass_counts = 0

		call sort(played_cards)
	
		if(has_pattern(played_cards) .eqv. .false.) then
			print*, "Illegal input!"
			cycle
		end if

		if(exists(current_player, played_cards) .eqv. .false.) then
			print*, "Play cards in your hand!"
			cycle
		end if

		if(is_empty(last_played_cards)) then
			! do nothing
		else if(pattern_latter_bigger(last_played_cards, played_cards)) then
			! do nothing
		else if(same_pattern(last_played_cards, played_cards) .and. latter_bigger(last_played_cards, played_cards)) then
			! do nothing
		else
			print*, "Player bigger than last guy!"
			cycle
		end if
	
		call remove(current_player, played_cards)
		if(is_empty(current_player)) then
			print*, "Winner is you!"
			exit
		end if
		last_played_cards = played_cards

		if(mod(counts,3) == 0) then
			lord%values(1:20) = current_player%values(1:20)
			call sort(lord)
		else if(mod(counts,3) == 1) then
			john%values(1:20) = current_player%values(1:20)
			call sort(john)
		else if(mod(counts,3) == 2) then
			mary%values(1:20) = current_player%values(1:20)
			call sort(mary)
		end if
		counts = counts + 1
		
	end do

end program test


subroutine remove(cards, sub_cards)
use constants
implicit none
	type(C), intent(inout) :: cards
	type(C), intent(in) :: sub_cards
	integer :: i

	do i = 1, 20
		if(sub_cards%values(i) /= 0) then
			call remove_single(cards, sub_cards%values(i))
		end if
	end do

end subroutine remove

subroutine remove_single(cards, card)
use constants
implicit none
	integer, intent(in) :: card
	type(C), intent(inout) :: cards
	integer :: i

	do i = 1, 20
		if(cards%values(i) == card) then
			cards%values(i) = 0
			exit
		end if
	end do

end subroutine remove_single

subroutine sort(cards)
use constants
implicit none
	type(C), intent(inout) :: cards
	integer :: i,j
	integer,dimension(20) :: tmp_values
	integer,dimension(20) :: sorted_values 

	sorted_values = (/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)

	tmp_values = cards%values

	do i=1,20
		if(tmp_values(i) == 0) then
			tmp_values(i) = 99
		end if
	end do

	j = 1
	do while(MINVAL(tmp_values) < 99)
		sorted_values(j) = MINVAL(tmp_values)
		tmp_values(MINLOC(tmp_values)) = 99
		j = j + 1
	end do

	do i=1,20
		cards%values(i) = sorted_values(i)
	end do
end subroutine sort

subroutine show(cards)
use constants
implicit none
	type(C), intent(in) :: cards
	integer :: i,j
	character(24) :: output
	output = "                        "

	call sync_from_value(cards)
	
	j = 1
	do i = 1,20
		if(cards%names(i:i) == "T") then
			output(j:j+1) = "10"
			j = j + 2
		else
			output(j:j) = cards%names(i:i)
			j = j + 1
		end if
	end do

	print*, output

end subroutine show

subroutine debug_show(cards)
use constants
implicit none
	type(C), intent(in) :: cards
	integer :: v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20
	v1 = cards%values(1)
	v2 = cards%values(2)
	v3 = cards%values(3)
	v4 = cards%values(4)
	v5 = cards%values(5)
	v6 = cards%values(6)
	v7 = cards%values(7)
	v8 = cards%values(8)
	v9 = cards%values(9)
	v10 = cards%values(10)
	v11 = cards%values(11)
	v12 = cards%values(12)
	v13 = cards%values(13)
	v14 = cards%values(14)
	v15 = cards%values(15)
	v16 = cards%values(16)
	v17 = cards%values(17)
	v18 = cards%values(18)
	v19 = cards%values(19)
	v20 = cards%values(20)
	call sync_from_value(cards)
	print*, cards%names
	print "(20i2)", v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20
end subroutine debug_show

subroutine sync_from_value(cards)
use constants
implicit none
	type(C), intent(out) :: cards
	integer :: i,j
	character :: value_to_name
	cards%names = 	"                    "
	j = 1
	do i = 1, 20
		if(cards%values(i) /= 0) then
			cards%names(j:j) = value_to_name(cards%values(i))
			j = j + 1
		end if
	end do 	
end subroutine sync_from_value

subroutine sync_from_name(cards)
use constants
implicit none
	type(C), intent(out) :: cards
	integer :: i, name_to_value
	cards%values = 	(/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
	do i = 1, 20
		if(cards%names(i:i) /= " ") then
			cards%values(i) = name_to_value(cards%names(i:i))
		end if
	end do 	
end subroutine sync_from_name

subroutine wait_input(cards)
use constants
implicit none
	type(C), intent(out) :: cards
	character(len=30) :: input
	integer :: input_size, i,j
	read*, input

	if(input(1:1) /= "p") then
		input_size = len(trim(input))
		j = 1
		do i = 1, input_size
			if(input(i:i) == "0") then
				cycle
			else if(input(i:i) == "1") then
				cards%names(j:j) = "T"
			else
				cards%names(j:j) = input(i:i)
			end if
			j = j + 1
		end do
		call sync_from_name(cards)		
	end if
end subroutine wait_input

function name_to_value(card_name)
use constants
implicit none
	integer :: name_to_value
	character :: card_name
	name_to_value = CARD_VALUE_LIST(index(CARD_NAME_LIST,card_name))
end function name_to_value

function value_to_name(card_value)
use constants
implicit none
	character :: value_to_name
	integer :: card_value

	integer :: i
	i = 1
	do while (CARD_VALUE_LIST(i) /= card_value)
		i = i + 1
	end do

	value_to_name = CARD_NAME_LIST(i:i)
end function value_to_name


