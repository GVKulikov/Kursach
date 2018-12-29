        INTEGER a(0:15), b(0:15), c(0:3,0:3), d(0:15),f(0:1,0:15);
        INTEGER i, j, k, l, m, n ,o ,p, x, max;
        a(0) = 15; a(1) = 53; a(2) = 24; a(3) = 10; a(4) = 95;
         a(5) = 43; a(6) = 28; a(7) = 37;
        a(8) = 21; a(9) = 32; a(10) = 14; a(11) = 61; a(12) = 51;
         a(13) = 16; a(14) = 63; a(15) = 20;
         
        max = 15;
        WRITE(*,*) "Исходный массив";
        DO 1 i=0,max, 1
	write (*,"(i4,$)") a(i);
1      CONTINUE
       WRITE(*,*);
       WRITE(*,*) "Perevorot glavnoy i pobochnoi diagonaley matrizi";
       DO 2 i = 0, 3, 1
	c(0,i) = a(i);
	c(1,i) = a(i + 4);
	c(2,i) = a(i + 8);
	c(3,i) = a(i + 12);
   2   CONTINUE
       write(*,*) "До переворота";
       DO 3        i = 0, 3, 1
	DO 4 j = 0, 3, 1
		WRITE(*,'(i3,$)') c(i, j);
4       CONTINUE
	WRITE(*,*)
3      CONTINUE
       DO 5 i = 0, 3/2, 1
       DO 6 j = 0, 3/2, 1
		IF (i.EQ.j) GO TO 7
7                       x = c(i,j);
			c(i,j) = c(3-i,3-j);
			c(3-i,3-j) = x;
6       CONTINUE
5      CONTINUE
       DO 8 i = (3/2 + 1), 3, 1
	DO 9 j = 0, 3/2, 1
		IF(.NOT.(j.EQ.3-i))GO TO 9
			x = c(i,j);
			c(i,j) = c(j,i);
			c(j,i) = x;
9       CONTINUE
8       CONTINUE
        WRITE(*,*)"После переворота";
        DO 11 i = 0, 3, 1
	DO 12 j = 0, 3, 1
		WRITE(*,'(i3,$)') c(i, j);
12     CONTINUE
	WRITE(*,*)
11     CONTINUE
       WRITE(*,*)"Сумма элементов столбцов матрицы";
            DO 13 i = 0, 3, 1
	c(0,i) = a(i);
	c(1,i) = a(i + 4);
	c(2,i) = a(i + 8);
	c(3,i) = a(i + 12);
13     CONTINUE
       DO 14 i = 0, 3, 1
	DO 15 j = 0, 3, 1
		WRITE(*,'(i3,$)') c(i, j);
15     CONTINUE
	WRITE(*,*)
14     CONTINUE
       DO 16 i = 0, 3, 1
	p = 0;
	DO 17 j = 0, 3, 1
		p = p + c(j,i);
17     CONTINUE
	WRITE(*,*) "sum: (", i, ")", p;
16     CONTINUE
       WRITE(*,*) "Сумма элементов строк матрицы";
       DO 18 i = 0, 3, 1
	DO 19 j = 0, 3, 1
		WRITE(*,'(i3,$)') c(i, j);
19     CONTINUE
	WRITE(*,*)
18     CONTINUE
       DO 20 i = 0, 3, 1
	p = 0;
	DO 21 j = 0, 3, 1
		p = p + c(i,j);
21     CONTINUE
	WRITE(*,*) "sum: (", i, ")", p;
20     CONTINUE
        WRITE(*,*) "кол-во элем в массиве отличающихся от мин на 5";
               DO 22 i = 0, max, 1
	b(i) = a(i);
	WRITE(*,'(i3,$)') b(i);
22      CONTINUE
        WRITE(*,*);
        k = b(0);
        DO 23 i = 1, max, 1


	IF (b(i).LT.k) k = b(i);

23     CONTINUE


       WRITE(*,*) "MIN: ", k;


       m = 0;

       DO 25 i = 0, max, 1



	IF ((b(i).EQ.(k + 5)).OR.(b(i).EQ.(k - 5))) m = m + 1;





25     CONTINUE


       WRITE(*,*) m;


       WRITE(*,*) "Сортировка вставками";


       DO 27 i = 1, max, 1

	k = i;
	x = b(i);

28     CONTINUE
		IF(.NOT.((k.GT.0).AND.(b(k - 1).GT.x))) GO TO 29

			b(k) = b(k-1);
			k = k - 1;

		GO TO 28
29     CONTINUE

	b(k) = x;


27     CONTINUE

        DO 30 i = 0, max, 1


	WRITE(*,'(i3,$)') b(i);

30     CONTINUE
       WRITE(*,*);

       DO 31 i = 0, max, 1

	b(i) = a(i);


31     CONTINUE


       WRITE(*,*) "Сортировка выбором";


       DO 32 i = 0, max - 1, 1

	m = i;
	x = b(i);

	DO 33 j = i + 1, max, 1


		IF (.NOT.(x.GT.b(j))) GO TO 33
			m = j;
			x = b(j);


33       CONTINUE

	b(m) = b(i);
	b(i) = x;


32      CONTINUE
        DO 35 i = 0, max, 1


	WRITE(*,'(i3,$)') b(i);


35     CONTINUE
       WRITE(*,*);

       DO 36 i = 0, max, 1


	b(i) = a(i);


36      CONTINUE



        WRITE(*,*) "Сортировка пузырьком";


        DO 37 i = max, 1, -1


	DO 38 j = 1, i, 1


		if(.NOT.(b(j).LT.b(j - 1))) GO TO 38
			x = b(j);
			b(j) = b(j - 1);
			b(j - 1) = x;


38       CONTINUE



37              CONTINUE
                DO 40 i = 0, max, 1


	WRITE(*,'(i3,$)') b(i);


40      CONTINUE
        WRITE(*,*);


        DO 41 i = 0, max, 1


	b(i) = a(i);


41     CONTINUE



       WRITE(*,*)"Найти мин элем ниже побочной диагонали";



       DO 42 i = 0, 3, 1


	DO 43 j = 0, 3, 1


		WRITE(*,'(i3,$)') c(i, j);



43       CONTINUE
	WRITE(*,*)

42     CONTINUE

       k = 35135;


         DO 44 i = 0, 3, 1


	DO 45 j = 3 - i + 1, 3, 1



		if (k.GT.c(i,j)) k=c(i,j);






45       CONTINUE


44       CONTINUE


         WRITE(*,*) k;



         WRITE(*,*) "Определить кол-во элементов массива > соседних";


         DO 46 i = 0, max, 1


	WRITE(*,'(i3,$)') b(i);


46     CONTINUE
       WRITE(*,*);
       k = 0;
       i = 1;

48    CONTINUE
        IF (.NOT.(i < max)) GOTO 47


	IF (.NOT.((b(i) > b(i-1)).AND.(b(i)>b(i+1)))) GO TO 49
				k = k + 1;
				i = i + 1;
49                              i = i + 1;
        GOTO 48
47    CONTINUE


      write(*,*) k;


      WRITE(*,*) "Минимальное элементы на пересечениии строк и ст";



      DO 50 i = 0, 3, 1


	DO 51 j =0, 3, 1


		WRITE(*,'(i3,$)') c(i,j);



51      CONTINUE
	WRITE(*,*);


50     CONTINUE
       n = 0;


       DO 52 i = 0, 3, 1


	m = 0;



	DO 53 j = 1, 3, 1


		if(c(i,j).LT.c(i, m)) m=j;






53     CONTINUE


	p = 1;


	DO 54 k = 0, max, 1


		if(.NOT.(c(k, m).LT.c(i, m))) GO TO 54
			p = 0;
			exit




54       CONTINUE

          if (.NOT.(p.EQ.1)) GO TO 52



		write(*,*) m;




		n = 1;






52     CONTINUE




56     IF (n.NE.1) WRITE(*,*) "Takix elementov net";








       WRITE(*,*) "Четеные и нечетные числа матрицы";





       p = -1;
       m =0;
       DO 57 i = 0, 3, 1



	DO 58 j = 0, 3, 1



	IF (.NOT.(MOD(c(i,j),2).NE.0)) GO TO 58


		WRITE(*,'(i3,$)') c(i,j);



		m = m + 1;
		p = p + 1;


	IF((p.EQ.3).OR.(p.EQ.7).OR.(p.EQ.11).OR.(p.EQ.15)) WRITE(*,*);









58      CONTINUE



57      CONTINUE
        n = 0;


        DO 59 i = 0, 3, 1



         DO 60 j = 0, 3, 1


          if (.NOT.(MOD(c(i,j),2).EQ.0)) GO TO 60


      WRITE(*,'(i3,$)') c(i,j);


	n = n + 1;
	p = p + 1;



	IF ((p.EQ.3).OR.(p.EQ.7).OR.(p.EQ.11).OR.(p.EQ.15)) WRITE(*,*);







  60   CONTINUE



59     CONTINUE
       WRITE(*,*) "4etnix: ", m;



       write(*,*) "ne4etnix: ", n;



       WRITE(*,*) "Сортировка шейкером";



       k = 1;
       j = max;



61    CONTINUE
        IF (.NOT. (k.LE.j)) GOTO 62


			DO 63 i = k, j, 1



         if(.NOT.(b(i).LE.b(i-1)))GO TO 63



				x = b(i);
				b(i) = b(i-1);
				b(i - 1) = x;



63        CONTINUE


		DO 64 i = j, 1, -1




		if(.NOT.(b(i).LE.b(i-1)))GO TO 64

				x = b(i);
				b(i) = b(i-1);
				b(i - 1) = x;



64     CONTINUE




		k = k + 1;
        GOTO 61



62    CONTINUE




      DO 65 i = 0, max, 1



	WRITE(*,'(i3,$)') b(i);




65     CONTINUE
       WRITE(*,*);
       WRITE(*,*) "Сортировка столбцов матрицы по убыванию эл в первой";



       DO 66 i = 0, max, 1



	f(0,i) = a(i);
	f(1,i) = a(max - i);



66     CONTINUE


       WRITE(*,*)"До сортировки";



       DO 67 i = 0, 1, 1



	DO 68 j = 0, max, 1



		WRITE(*,'(i3,$)') f(i, j);



68     CONTINUE
	WRITE(*,*);


67     CONTINUE

       k = max;
       j = 0;

69    CONTINUE
        IF (.NOT.(k.GE.0)) GOTO 70

			p = f(0,0);
			u = 0;

			DO 71 j = 1, k - 1, 1


				if (.NOT.(f(0,j).LE.p)) GO TO 71

					p=f(0,j);
					u = j;


71      CONTINUE


			f(0,u) = f(0,j);
			f(0,j) = p;

			DO 72 i = 1, 1, 1


				p = f(i,u);
				f(i,u) = f(i,j)
				f(i,j) = p;


72        CONTINUE

			k = k - 1;


        GOTO 69
70    CONTINUE
      WRITE(*,*) "После сортировки";


      DO 73 i = 0, 1, 1



	DO 74 j = 0, max, 1


		WRITE(*,'(i3,$)') f(i, j);


74     CONTINUE
	WRITE(*,*);


73     CONTINUE


       WRITE(*,*)"Функция циклического сдвига";


       WRITE(*,*) "До сдвига";


       DO 75 i = 0, max, 1



	WRITE(*,'(i3,$)') b(i);



75     CONTINUE
       WRITE(*,*);

       p = 6;
       p = p - 1;
       DO 76 j = 0, p, 1



	k = b(max);



	DO 77 i = max, 1, -1


		b(i) = b(i - 1);




77     CONTINUE


	b(0) = k;



76     CONTINUE


       WRITE(*,*) "После сдвига";



       DO 78 i = 0, max, 1



	WRITE(*,'(i3,$)') b(i);



78     CONTINUE
       WRITE(*,*);
       WRITE(*,*)"Вывести значения массива по спирали";


       DO 79 i = 0, max, 1


	WRITE(*,'(i3,$)') b(i);



79     CONTINUE
       WRITE(*,*);

       DO 80 i = 0, 3, 1



          IF(.NOT.(MOD(i,2).EQ.0))GO TO 81

		DO 82 j = i*4, i*4 + 3, 1
		   WRITE(*,'(i3,$)') b(j);
82      CONTINUE
		WRITE(*,*);
		GO TO 80
81     DO 83 j = i*4 + 3, i*4, -1
			WRITE(*,'(i3,$)') b(j);
83       CONTINUE
		WRITE(*,*);





80       CONTINUE
         pause
         stop
         end

