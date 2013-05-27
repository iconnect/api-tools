-----------------------------------------------------------------------------
Info file generated by Happy Version 1.18.9 from Data/API/Parse.y
-----------------------------------------------------------------------------

state 14 contains 1 shift/reduce conflicts.

-----------------------------------------------------------------------------
Grammar
-----------------------------------------------------------------------------
	%start_parse -> API                                (0)
	%start_parse_elt -> RUFields                       (1)
	API -> RAPI                                        (2)
	RAPI -> RAPI ';' Thing                             (3)
	RAPI ->                                            (4)
	Thing -> Node                                      (5)
	Thing -> Comments                                  (6)
	Node -> Prefix '::' typeiden Comments '=' Spec With Vrn Comments   (7)
	Spec -> Record                                     (8)
	Spec -> Union                                      (9)
	Spec -> Enum                                       (10)
	Spec -> Type                                       (11)
	With -> with FieldName ',' FieldName               (12)
	With ->                                            (13)
	Vrn -> version intlit                              (14)
	Vrn ->                                             (15)
	Comments -> RCommentList                           (16)
	RCommentList -> RCommentList comment               (17)
	RCommentList ->                                    (18)
	Prefix -> variden                                  (19)
	Record -> record RRFields                          (20)
	Union -> union RUFields                            (21)
	RRFields -> RRFields FieldName ':' Type Comments   (22)
	RRFields -> FieldName ':' Type Comments            (23)
	RUFields -> RUFields '|' FieldName ':' Type Comments   (24)
	RUFields -> '|' FieldName ':' Type Comments        (25)
	Enum -> REnums                                     (26)
	REnums -> REnums '|' FieldName                     (27)
	REnums -> FieldName                                (28)
	Type -> '?' Type                                   (29)
	Type -> '[' Type ']'                               (30)
	Type -> typeiden                                   (31)
	Type -> Basic                                      (32)
	Basic -> string                                    (33)
	Basic -> binary                                    (34)
	Basic -> boolean                                   (35)
	Basic -> integer                                   (36)
	Basic -> utc                                       (37)
	FieldName -> variden                               (38)

-----------------------------------------------------------------------------
Terminals
-----------------------------------------------------------------------------
	';'            { (,) _ Semi }
	'|'            { (,) _ Bar }
	'['            { (,) _ Bra }
	']'            { (,) _ Ket }
	'::'           { (,) _ ColCol }
	':'            { (,) _ Colon }
	'='            { (,) _ Equals }
	'?'            { (,) _ Query }
	','            { (,) _ Comma }
	version        { (,) _ Version }
	with           { (,) _ With }
	integer        { (,) _ Integer }
	boolean        { (,) _ Boolean }
	utc            { (,) _ UTC }
	string         { (,) _ String }
	binary         { (,) _ Binary }
	record         { (,) _ Record }
	union          { (,) _ Union }
	comment        { (,) _ (Comment  $$) }
	typeiden       { (,) _ (TypeIden $$) }
	variden        { (,) _ (VarIden  $$) }
	intlit         { (,) _ (Intg     $$) }

-----------------------------------------------------------------------------
Non-terminals
-----------------------------------------------------------------------------
	%start_parse    rule  0
	%start_parse_elt rule  1
	API             rule  2
	RAPI            rules 3, 4
	Thing           rules 5, 6
	Node            rule  7
	Spec            rules 8, 9, 10, 11
	With            rules 12, 13
	Vrn             rules 14, 15
	Comments        rule  16
	RCommentList    rules 17, 18
	Prefix          rule  19
	Record          rule  20
	Union           rule  21
	RRFields        rules 22, 23
	RUFields        rules 24, 25
	Enum            rule  26
	REnums          rules 27, 28
	Type            rules 29, 30, 31, 32
	Basic           rules 33, 34, 35, 36, 37
	FieldName       rule  38

-----------------------------------------------------------------------------
States
-----------------------------------------------------------------------------
State 0


	';'            reduce using rule 4
	%eof           reduce using rule 4

	API            goto state 6
	RAPI           goto state 3

State 1


	'|'            shift, and enter state 5

	RUFields       goto state 4

State 2



	RAPI           goto state 3

State 3

	API -> RAPI .                                       (rule 2)
	RAPI -> RAPI . ';' Thing                            (rule 3)

	';'            shift, and enter state 10
	%eof           reduce using rule 2


State 4

	%start_parse_elt -> RUFields .                      (rule 1)
	RUFields -> RUFields . '|' FieldName ':' Type Comments    (rule 24)

	'|'            shift, and enter state 9
	%eof           accept


State 5

	RUFields -> '|' . FieldName ':' Type Comments       (rule 25)

	variden        shift, and enter state 8

	FieldName      goto state 7

State 6

	%start_parse -> API .                               (rule 0)

	%eof           accept


State 7

	RUFields -> '|' FieldName . ':' Type Comments       (rule 25)

	':'            shift, and enter state 18


State 8

	FieldName -> variden .                              (rule 38)

	';'            reduce using rule 38
	'|'            reduce using rule 38
	':'            reduce using rule 38
	','            reduce using rule 38
	version        reduce using rule 38
	with           reduce using rule 38
	comment        reduce using rule 38
	%eof           reduce using rule 38


State 9

	RUFields -> RUFields '|' . FieldName ':' Type Comments    (rule 24)

	variden        shift, and enter state 8

	FieldName      goto state 17

State 10

	RAPI -> RAPI ';' . Thing                            (rule 3)

	';'            reduce using rule 18
	comment        reduce using rule 18
	variden        shift, and enter state 16
	%eof           reduce using rule 18

	Thing          goto state 11
	Node           goto state 12
	Comments       goto state 13
	RCommentList   goto state 14
	Prefix         goto state 15

State 11

	RAPI -> RAPI ';' Thing .                            (rule 3)

	';'            reduce using rule 3
	%eof           reduce using rule 3


State 12

	Thing -> Node .                                     (rule 5)

	';'            reduce using rule 5
	%eof           reduce using rule 5


State 13

	Thing -> Comments .                                 (rule 6)

	';'            reduce using rule 6
	%eof           reduce using rule 6


State 14

	Comments -> RCommentList .                          (rule 16)
	RCommentList -> RCommentList . comment              (rule 17)

	';'            reduce using rule 16
	'|'            reduce using rule 16
	'='            reduce using rule 16
	version        reduce using rule 16
	with           reduce using rule 16
	comment        shift, and enter state 31
			(reduce using rule 16)

	variden        reduce using rule 16
	%eof           reduce using rule 16


State 15

	Node -> Prefix . '::' typeiden Comments '=' Spec With Vrn Comments    (rule 7)

	'::'           shift, and enter state 30


State 16

	Prefix -> variden .                                 (rule 19)

	'::'           reduce using rule 19


State 17

	RUFields -> RUFields '|' FieldName . ':' Type Comments    (rule 24)

	':'            shift, and enter state 29


State 18

	RUFields -> '|' FieldName ':' . Type Comments       (rule 25)

	'['            shift, and enter state 21
	'?'            shift, and enter state 22
	integer        shift, and enter state 23
	boolean        shift, and enter state 24
	utc            shift, and enter state 25
	string         shift, and enter state 26
	binary         shift, and enter state 27
	typeiden       shift, and enter state 28

	Type           goto state 19
	Basic          goto state 20

State 19

	RUFields -> '|' FieldName ':' Type . Comments       (rule 25)

	';'            reduce using rule 18
	'|'            reduce using rule 18
	version        reduce using rule 18
	with           reduce using rule 18
	comment        reduce using rule 18
	%eof           reduce using rule 18

	Comments       goto state 36
	RCommentList   goto state 14

State 20

	Type -> Basic .                                     (rule 32)

	';'            reduce using rule 32
	'|'            reduce using rule 32
	']'            reduce using rule 32
	version        reduce using rule 32
	with           reduce using rule 32
	comment        reduce using rule 32
	variden        reduce using rule 32
	%eof           reduce using rule 32


State 21

	Type -> '[' . Type ']'                              (rule 30)

	'['            shift, and enter state 21
	'?'            shift, and enter state 22
	integer        shift, and enter state 23
	boolean        shift, and enter state 24
	utc            shift, and enter state 25
	string         shift, and enter state 26
	binary         shift, and enter state 27
	typeiden       shift, and enter state 28

	Type           goto state 35
	Basic          goto state 20

State 22

	Type -> '?' . Type                                  (rule 29)

	'['            shift, and enter state 21
	'?'            shift, and enter state 22
	integer        shift, and enter state 23
	boolean        shift, and enter state 24
	utc            shift, and enter state 25
	string         shift, and enter state 26
	binary         shift, and enter state 27
	typeiden       shift, and enter state 28

	Type           goto state 34
	Basic          goto state 20

State 23

	Basic -> integer .                                  (rule 36)

	';'            reduce using rule 36
	'|'            reduce using rule 36
	']'            reduce using rule 36
	version        reduce using rule 36
	with           reduce using rule 36
	comment        reduce using rule 36
	variden        reduce using rule 36
	%eof           reduce using rule 36


State 24

	Basic -> boolean .                                  (rule 35)

	';'            reduce using rule 35
	'|'            reduce using rule 35
	']'            reduce using rule 35
	version        reduce using rule 35
	with           reduce using rule 35
	comment        reduce using rule 35
	variden        reduce using rule 35
	%eof           reduce using rule 35


State 25

	Basic -> utc .                                      (rule 37)

	';'            reduce using rule 37
	'|'            reduce using rule 37
	']'            reduce using rule 37
	version        reduce using rule 37
	with           reduce using rule 37
	comment        reduce using rule 37
	variden        reduce using rule 37
	%eof           reduce using rule 37


State 26

	Basic -> string .                                   (rule 33)

	';'            reduce using rule 33
	'|'            reduce using rule 33
	']'            reduce using rule 33
	version        reduce using rule 33
	with           reduce using rule 33
	comment        reduce using rule 33
	variden        reduce using rule 33
	%eof           reduce using rule 33


State 27

	Basic -> binary .                                   (rule 34)

	';'            reduce using rule 34
	'|'            reduce using rule 34
	']'            reduce using rule 34
	version        reduce using rule 34
	with           reduce using rule 34
	comment        reduce using rule 34
	variden        reduce using rule 34
	%eof           reduce using rule 34


State 28

	Type -> typeiden .                                  (rule 31)

	';'            reduce using rule 31
	'|'            reduce using rule 31
	']'            reduce using rule 31
	version        reduce using rule 31
	with           reduce using rule 31
	comment        reduce using rule 31
	variden        reduce using rule 31
	%eof           reduce using rule 31


State 29

	RUFields -> RUFields '|' FieldName ':' . Type Comments    (rule 24)

	'['            shift, and enter state 21
	'?'            shift, and enter state 22
	integer        shift, and enter state 23
	boolean        shift, and enter state 24
	utc            shift, and enter state 25
	string         shift, and enter state 26
	binary         shift, and enter state 27
	typeiden       shift, and enter state 28

	Type           goto state 33
	Basic          goto state 20

State 30

	Node -> Prefix '::' . typeiden Comments '=' Spec With Vrn Comments    (rule 7)

	typeiden       shift, and enter state 32


State 31

	RCommentList -> RCommentList comment .              (rule 17)

	';'            reduce using rule 17
	'|'            reduce using rule 17
	'='            reduce using rule 17
	version        reduce using rule 17
	with           reduce using rule 17
	comment        reduce using rule 17
	variden        reduce using rule 17
	%eof           reduce using rule 17


State 32

	Node -> Prefix '::' typeiden . Comments '=' Spec With Vrn Comments    (rule 7)

	'='            reduce using rule 18
	comment        reduce using rule 18

	Comments       goto state 39
	RCommentList   goto state 14

State 33

	RUFields -> RUFields '|' FieldName ':' Type . Comments    (rule 24)

	';'            reduce using rule 18
	'|'            reduce using rule 18
	version        reduce using rule 18
	with           reduce using rule 18
	comment        reduce using rule 18
	%eof           reduce using rule 18

	Comments       goto state 38
	RCommentList   goto state 14

State 34

	Type -> '?' Type .                                  (rule 29)

	';'            reduce using rule 29
	'|'            reduce using rule 29
	']'            reduce using rule 29
	version        reduce using rule 29
	with           reduce using rule 29
	comment        reduce using rule 29
	variden        reduce using rule 29
	%eof           reduce using rule 29


State 35

	Type -> '[' Type . ']'                              (rule 30)

	']'            shift, and enter state 37


State 36

	RUFields -> '|' FieldName ':' Type Comments .       (rule 25)

	';'            reduce using rule 25
	'|'            reduce using rule 25
	version        reduce using rule 25
	with           reduce using rule 25
	comment        reduce using rule 25
	%eof           reduce using rule 25


State 37

	Type -> '[' Type ']' .                              (rule 30)

	';'            reduce using rule 30
	'|'            reduce using rule 30
	']'            reduce using rule 30
	version        reduce using rule 30
	with           reduce using rule 30
	comment        reduce using rule 30
	variden        reduce using rule 30
	%eof           reduce using rule 30


State 38

	RUFields -> RUFields '|' FieldName ':' Type Comments .    (rule 24)

	';'            reduce using rule 24
	'|'            reduce using rule 24
	version        reduce using rule 24
	with           reduce using rule 24
	comment        reduce using rule 24
	%eof           reduce using rule 24


State 39

	Node -> Prefix '::' typeiden Comments . '=' Spec With Vrn Comments    (rule 7)

	'='            shift, and enter state 40


State 40

	Node -> Prefix '::' typeiden Comments '=' . Spec With Vrn Comments    (rule 7)

	'['            shift, and enter state 21
	'?'            shift, and enter state 22
	integer        shift, and enter state 23
	boolean        shift, and enter state 24
	utc            shift, and enter state 25
	string         shift, and enter state 26
	binary         shift, and enter state 27
	record         shift, and enter state 48
	union          shift, and enter state 49
	typeiden       shift, and enter state 28
	variden        shift, and enter state 8

	Spec           goto state 41
	Record         goto state 42
	Union          goto state 43
	Enum           goto state 44
	REnums         goto state 45
	Type           goto state 46
	Basic          goto state 20
	FieldName      goto state 47

State 41

	Node -> Prefix '::' typeiden Comments '=' Spec . With Vrn Comments    (rule 7)

	';'            reduce using rule 13
	version        reduce using rule 13
	with           shift, and enter state 55
	comment        reduce using rule 13
	%eof           reduce using rule 13

	With           goto state 54

State 42

	Spec -> Record .                                    (rule 8)

	';'            reduce using rule 8
	version        reduce using rule 8
	with           reduce using rule 8
	comment        reduce using rule 8
	%eof           reduce using rule 8


State 43

	Spec -> Union .                                     (rule 9)

	';'            reduce using rule 9
	version        reduce using rule 9
	with           reduce using rule 9
	comment        reduce using rule 9
	%eof           reduce using rule 9


State 44

	Spec -> Enum .                                      (rule 10)

	';'            reduce using rule 10
	version        reduce using rule 10
	with           reduce using rule 10
	comment        reduce using rule 10
	%eof           reduce using rule 10


State 45

	Enum -> REnums .                                    (rule 26)
	REnums -> REnums . '|' FieldName                    (rule 27)

	';'            reduce using rule 26
	'|'            shift, and enter state 53
	version        reduce using rule 26
	with           reduce using rule 26
	comment        reduce using rule 26
	%eof           reduce using rule 26


State 46

	Spec -> Type .                                      (rule 11)

	';'            reduce using rule 11
	version        reduce using rule 11
	with           reduce using rule 11
	comment        reduce using rule 11
	%eof           reduce using rule 11


State 47

	REnums -> FieldName .                               (rule 28)

	';'            reduce using rule 28
	'|'            reduce using rule 28
	version        reduce using rule 28
	with           reduce using rule 28
	comment        reduce using rule 28
	%eof           reduce using rule 28


State 48

	Record -> record . RRFields                         (rule 20)

	variden        shift, and enter state 8

	RRFields       goto state 51
	FieldName      goto state 52

State 49

	Union -> union . RUFields                           (rule 21)

	'|'            shift, and enter state 5

	RUFields       goto state 50

State 50

	Union -> union RUFields .                           (rule 21)
	RUFields -> RUFields . '|' FieldName ':' Type Comments    (rule 24)

	';'            reduce using rule 21
	'|'            shift, and enter state 9
	version        reduce using rule 21
	with           reduce using rule 21
	comment        reduce using rule 21
	%eof           reduce using rule 21


State 51

	Record -> record RRFields .                         (rule 20)
	RRFields -> RRFields . FieldName ':' Type Comments    (rule 22)

	';'            reduce using rule 20
	version        reduce using rule 20
	with           reduce using rule 20
	comment        reduce using rule 20
	variden        shift, and enter state 8
	%eof           reduce using rule 20

	FieldName      goto state 61

State 52

	RRFields -> FieldName . ':' Type Comments           (rule 23)

	':'            shift, and enter state 60


State 53

	REnums -> REnums '|' . FieldName                    (rule 27)

	variden        shift, and enter state 8

	FieldName      goto state 59

State 54

	Node -> Prefix '::' typeiden Comments '=' Spec With . Vrn Comments    (rule 7)

	';'            reduce using rule 15
	version        shift, and enter state 58
	comment        reduce using rule 15
	%eof           reduce using rule 15

	Vrn            goto state 57

State 55

	With -> with . FieldName ',' FieldName              (rule 12)

	variden        shift, and enter state 8

	FieldName      goto state 56

State 56

	With -> with FieldName . ',' FieldName              (rule 12)

	','            shift, and enter state 66


State 57

	Node -> Prefix '::' typeiden Comments '=' Spec With Vrn . Comments    (rule 7)

	';'            reduce using rule 18
	comment        reduce using rule 18
	%eof           reduce using rule 18

	Comments       goto state 65
	RCommentList   goto state 14

State 58

	Vrn -> version . intlit                             (rule 14)

	intlit         shift, and enter state 64


State 59

	REnums -> REnums '|' FieldName .                    (rule 27)

	';'            reduce using rule 27
	'|'            reduce using rule 27
	version        reduce using rule 27
	with           reduce using rule 27
	comment        reduce using rule 27
	%eof           reduce using rule 27


State 60

	RRFields -> FieldName ':' . Type Comments           (rule 23)

	'['            shift, and enter state 21
	'?'            shift, and enter state 22
	integer        shift, and enter state 23
	boolean        shift, and enter state 24
	utc            shift, and enter state 25
	string         shift, and enter state 26
	binary         shift, and enter state 27
	typeiden       shift, and enter state 28

	Type           goto state 63
	Basic          goto state 20

State 61

	RRFields -> RRFields FieldName . ':' Type Comments    (rule 22)

	':'            shift, and enter state 62


State 62

	RRFields -> RRFields FieldName ':' . Type Comments    (rule 22)

	'['            shift, and enter state 21
	'?'            shift, and enter state 22
	integer        shift, and enter state 23
	boolean        shift, and enter state 24
	utc            shift, and enter state 25
	string         shift, and enter state 26
	binary         shift, and enter state 27
	typeiden       shift, and enter state 28

	Type           goto state 69
	Basic          goto state 20

State 63

	RRFields -> FieldName ':' Type . Comments           (rule 23)

	';'            reduce using rule 18
	version        reduce using rule 18
	with           reduce using rule 18
	comment        reduce using rule 18
	variden        reduce using rule 18
	%eof           reduce using rule 18

	Comments       goto state 68
	RCommentList   goto state 14

State 64

	Vrn -> version intlit .                             (rule 14)

	';'            reduce using rule 14
	comment        reduce using rule 14
	%eof           reduce using rule 14


State 65

	Node -> Prefix '::' typeiden Comments '=' Spec With Vrn Comments .    (rule 7)

	';'            reduce using rule 7
	%eof           reduce using rule 7


State 66

	With -> with FieldName ',' . FieldName              (rule 12)

	variden        shift, and enter state 8

	FieldName      goto state 67

State 67

	With -> with FieldName ',' FieldName .              (rule 12)

	';'            reduce using rule 12
	version        reduce using rule 12
	comment        reduce using rule 12
	%eof           reduce using rule 12


State 68

	RRFields -> FieldName ':' Type Comments .           (rule 23)

	';'            reduce using rule 23
	version        reduce using rule 23
	with           reduce using rule 23
	comment        reduce using rule 23
	variden        reduce using rule 23
	%eof           reduce using rule 23


State 69

	RRFields -> RRFields FieldName ':' Type . Comments    (rule 22)

	';'            reduce using rule 18
	version        reduce using rule 18
	with           reduce using rule 18
	comment        reduce using rule 18
	variden        reduce using rule 18
	%eof           reduce using rule 18

	Comments       goto state 70
	RCommentList   goto state 14

State 70

	RRFields -> RRFields FieldName ':' Type Comments .    (rule 22)

	';'            reduce using rule 22
	version        reduce using rule 22
	with           reduce using rule 22
	comment        reduce using rule 22
	variden        reduce using rule 22
	%eof           reduce using rule 22


-----------------------------------------------------------------------------
Grammar Totals
-----------------------------------------------------------------------------
Number of rules: 39
Number of terminals: 22
Number of non-terminals: 21
Number of states: 71
