!      Game of hangman by dave ahl, digital
!      based on a basic program written by Ken Aupperle
!            Half Hallow Hills H.H. Dix Hills NY
!      Converted to fortran 77 by M.Wirth, April 2012
!      Converted again to fortran 95 by Gregory Campbell, February 2016
!       
 program hangman
       implicit none
       character :: p(12,12)
       character :: d(20), n(26), a*20, guess, b*20, ans 
       integer :: u(50)
       integer :: q, m, i, j, w, t1, r, l, c, win = 0, dead = 0, wrong = 0  

       character(len=20), dimension(50) :: dict = [character(len=20) ::'gum','sin','for','cry','lug','bye','fly',& 
     &           'ugly','each','from','work','talk','with','self',&
     &           'pizza','thing','feign','fiend','elbow','fault',&
     &           'dirty','budget','spirit','quaint','maiden',&
     &           'escort','pickax','example','tension','quinine',&
     &           'kidney','replica','sleeper','triangle',&
     &           'kangaroo','mahogany','sergeant','sequence',&
     &           'moustache','dangerous','scientist','different',&
     &           'quiescent','magistrate','erroneously',&
     &           'loudspeaker','phytotoxic','matrimonial',&
     &           'parasympathomimetic','thigmotropism']
       write (*,*) "the game of hangman"
           do
           do i = 1,12
               do j = 1,12
                   p(i,j) = " "
               end do
           end do
           do i = 1,20
               d(i) = "-"
           end do
           do  i = 1,26
               n(i) = " "
           end do
           do i = 1,50
               u(i) = 0
           end do
           do i = 1,12
               p(i,1) = "x"
           end do
           do j = 1,7
               p(1,j) = "x"
           end do
           p(2,7) = "x"
           c=1
           w=50
           m=0 
           if (c >= w) then
               write (*,*) "you did all the words"; exit
           end if
           
           u(q) = 1; c=c+1; t1=0
                 
           do while ((u(q) - 1) == 0)
               q = random(q)
           end do
           
           
           a = dict(q)
           l = len_trim(a) 
           write (*,*) d(1:l)
           do while (win == 0)
               wrong = 0
               write (*,*) "here are the letters you used: "
               do i = 1,26
                   if (n(i) == ' ') then
                       exit
                   end if
                   write (*,'(aa$)') n(i),","
               end do
               write (*,*) " "
               write (*,*) "what is your guess? "; r=0
               read (*,*) guess 
               do i = 1,26
                   if (n(i) == " ") then
                       exit
                   end if 
                   if ((ichar(n(i)) - ichar(guess)) == 0) then
                       write (*,*) "you guessed that letter before";
                       exit
                   end if
               end do
               n(i)=guess; t1=t1+1
               do i = 1,l
                   if (a(i:i) == guess) then
                       d(i) = guess; r=r+1
                   end if
               end do
               do while (r == 0) 
                   m=m+1   
                   write (*,*) "sorry, that letter isn't in the word."
                   select case (m)
                       case(:1)
                           write (*,*) "first we draw a head.";
                           p(3,6) = "-"; p(3,7) = "-"; p(3,8) = "-"; p(4,5) = "("; 
                           p(4,6) = "."; p(4,8) = "."; p(4,9) = ")"; p(5,6) = "-"; 
                           p(5,7) = "-"; p(5,8) = "-";
                       case(2)
                           write (*,*) "now we draw a body.";
                           do i = 6,9
                               p(i,7) = "x" ;
                           end do
                       case(3)
                           write (*,*) "next we draw an arm.";
                           do i = 4,7
                               p(i,i-1) = "\";
                           end do
                       case(4)
                           write (*,*) "this time it's the other arm.";
                           p(4,11) = "/"; p(5,10) = "/"; p(6,9) = "/"; p(7,8) = "/";
                       case(5)
                           write (*,*) "now, let's draw the right leg.";
                           p(10,6) = "/"; p(11,5) = "/";
                       case(6)
                           write (*,*) "this time we draw the left leg.";
                           p(10,8) = "\"; p(11,9) = "\";
                       case(7)
                           write (*,*) "now we put up a hand.";
                           p(3,11) = "\";
                       case(8)
                           write (*,*) "next the other hand.";
                           p(3,3) = "/";
                       case(9)
                           write (*,*) "now we draw one foot.";
                           p(12,10) = "\"; p(12,11) = "-";
                       case(10:)
                           write (*,*) "here's the other foot -- you're hung!!.";
                           p(12,3) = "-"; p(12,4) = "/"
                           dead = 1
                   end select
                   do i = 1,12
                       write (*,*) (p(i,j),j=1,12)
                   end do
                   if ((m - 10) /= 0 .and. dead == 0) then
                       write (*,*) "here are the letters you used: "
                       do i = 1,26
                           if (n(i) == ' ') then
                               exit
                           end if
                           write (*,'(aa$)') n(i),","
                       end do
                       write (*,*) " "
                       write (*,*) "what is your guess? "; r=0
                       read (*,*) guess 
                       do i = 1,26
                           if (n(i) == " ") then
                               exit
                           end if 
                           if ((ichar(n(i)) - ichar(guess)) == 0) then
                               write (*,*) "you guessed that letter before";
                               exit
                           end if
                       end do
                       n(i)=guess; t1=t1+1
                       do i = 1,l
                           if (a(i:i) == guess) then
                               d(i) = guess; r=r+1
                           end if
                       end do
                   else 
                       write (*,*) "sorry, you loose. the word was ", a
                       write (*,*) "you missed that one."; exit
                   end if
               end do
               if (dead == 1) exit
               do i = 1,l
                   if ((ichar(d(i)) - ichar("-")) == 0) then
                       write (*,*) d(1:l)
                       write (*,*) "what is your guess for the word? "
                       read (*,*) b
                       if (a == b) then
                           write (*,*) "right! it took you ",t1," guesses"
                           win = 1 
                           wrong = 0
                           exit
                       else
                           write (*,*) "wrong. try another letter"
                           wrong = 1
                           exit
                       end if
                   end if
               end do
               if (wrong == 0) then
                   write (*,*) "you found the word."
                   win = 1
                   wrong = 0
               end if
           end do
           win = 0
           dead = 0
           write (*,*) "do you want another word? (y/n) "
           read (*,*) ans
           if ((ichar(ans) - ichar("y")) /= 0 .and. (ichar(ans) - ichar("Y")) /= 0) then
               write (*,*) "it's been fun! bye for now."; exit
           end if       
       end do
       write (*,*) "ending..." 

       contains
       
       integer function random(q)
       
           implicit none
           real :: rnd
           integer :: q
           rnd=rand()
           q=ceiling(rnd*50)
           
           random = q
           
       end function random
       
       end
