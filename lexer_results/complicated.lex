TOK_LPAREN
(TOK_ID define)
(TOK_ID print-endline)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID c)
TOK_RPAREN
TOK_LPAREN
(TOK_ID print)
(TOK_ID c)
TOK_RPAREN
TOK_LPAREN
(TOK_ID print)
(TOK_CHAR(Ok"\n"))
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID nil)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID a)
(TOK_ID b)
TOK_RPAREN
TOK_LPAREN
(TOK_ID a)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID cons)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID a)
(TOK_ID b)
TOK_RPAREN
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID c)
(TOK_ID d)
TOK_RPAREN
TOK_LPAREN
(TOK_ID d)
(TOK_ID a)
(TOK_ID b)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID car)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID list)
TOK_RPAREN
TOK_LPAREN
(TOK_ID list)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
TOK_RPAREN
TOK_LPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID a)
(TOK_ID b)
TOK_RPAREN
(TOK_ID a)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID cdr)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID list)
TOK_RPAREN
TOK_LPAREN
(TOK_ID list)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
TOK_RPAREN
TOK_LPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID a)
(TOK_ID b)
TOK_RPAREN
(TOK_ID b)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID nil?)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID list)
TOK_RPAREN
TOK_LPAREN
(TOK_ID if)
TOK_LPAREN
(TOK_ID list)
(TOK_BOOL true)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID a)
(TOK_ID b)
TOK_RPAREN
(TOK_BOOL false)
TOK_RPAREN
TOK_RPAREN
(TOK_BOOL true)
(TOK_BOOL false)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID cons?)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID list)
TOK_RPAREN
TOK_LPAREN
(TOK_ID if)
TOK_LPAREN
(TOK_ID list)
(TOK_BOOL true)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID a)
(TOK_ID b)
TOK_RPAREN
(TOK_BOOL false)
TOK_RPAREN
TOK_RPAREN
(TOK_BOOL false)
(TOK_BOOL true)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID curry)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID f)
TOK_RPAREN
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID a)
TOK_RPAREN
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID b)
TOK_RPAREN
TOK_LPAREN
(TOK_ID f)
(TOK_ID a)
(TOK_ID b)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID map)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID f)
(TOK_ID list)
TOK_RPAREN
TOK_LPAREN
(TOK_ID if)
TOK_LPAREN
(TOK_ID nil?)
(TOK_ID list)
TOK_RPAREN
(TOK_ID nil)
TOK_LPAREN
(TOK_ID cons)
TOK_LPAREN
(TOK_ID f)
TOK_LPAREN
(TOK_ID car)
(TOK_ID list)
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID map)
(TOK_ID f)
TOK_LPAREN
(TOK_ID cdr)
(TOK_ID list)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID ignore)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID x)
TOK_RPAREN
TOK_LPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID $)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID f1)
(TOK_ID f2)
TOK_RPAREN
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID x)
TOK_RPAREN
TOK_LPAREN
(TOK_ID f1)
TOK_LPAREN
(TOK_ID f2)
(TOK_ID x)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID $$)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID f1)
(TOK_ID f2)
TOK_RPAREN
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID x)
(TOK_ID y)
TOK_RPAREN
TOK_LPAREN
(TOK_ID f1)
TOK_LPAREN
(TOK_ID f2)
(TOK_ID x)
(TOK_ID y)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID iter)
TOK_LPAREN
(TOK_ID $$)
(TOK_ID ignore)
(TOK_ID map)
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID print-list)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID iter)
TOK_RPAREN
(TOK_ID print)
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID print-list)
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 1))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 2))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 3))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 4))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 5))
(TOK_ID nil)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID filter)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID pred?)
(TOK_ID list)
TOK_RPAREN
TOK_LPAREN
(TOK_ID if)
TOK_LPAREN
(TOK_ID nil?)
(TOK_ID list)
TOK_RPAREN
(TOK_ID nil)
TOK_LPAREN
(TOK_ID if)
TOK_LPAREN
(TOK_ID pred?)
TOK_LPAREN
(TOK_ID car)
(TOK_ID list)
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID cons)
TOK_LPAREN
(TOK_ID car)
(TOK_ID list)
TOK_RPAREN
TOK_LPAREN
(TOK_ID filter)
(TOK_ID pred?)
TOK_LPAREN
(TOK_ID cdr)
(TOK_ID list)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID filter)
(TOK_ID pred?)
TOK_LPAREN
(TOK_ID cdr)
(TOK_ID list)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID print-list)
TOK_LPAREN
(TOK_ID filter)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID x)
TOK_RPAREN
TOK_LPAREN
(TOK_ID <)
(TOK_ID x)
(TOK_INT(Ok 10))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 1))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 2))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 3))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 10))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 11))
(TOK_ID nil)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID ++)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID list1)
(TOK_ID list2)
TOK_RPAREN
TOK_LPAREN
(TOK_ID if)
TOK_LPAREN
(TOK_ID nil?)
(TOK_ID list1)
TOK_RPAREN
(TOK_ID list2)
TOK_LPAREN
(TOK_ID cons)
TOK_LPAREN
(TOK_ID car)
(TOK_ID list1)
TOK_RPAREN
TOK_LPAREN
(TOK_ID ++)
TOK_LPAREN
(TOK_ID cdr)
(TOK_ID list1)
TOK_RPAREN
(TOK_ID list2)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID print-list)
TOK_LPAREN
(TOK_ID ++)
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 1))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 2))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 3))
(TOK_ID nil)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 4))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 5))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 6))
(TOK_ID nil)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID print-list)
TOK_LPAREN
(TOK_ID ++)
(TOK_ID nil)
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 4))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 5))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 6))
(TOK_ID nil)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID quicksort)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID list)
TOK_RPAREN
TOK_LPAREN
(TOK_ID if)
TOK_LPAREN
(TOK_ID nil?)
(TOK_ID list)
TOK_RPAREN
(TOK_ID nil)
TOK_LPAREN
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID less?)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID x)
TOK_RPAREN
TOK_LPAREN
(TOK_ID <)
(TOK_ID x)
TOK_LPAREN
(TOK_ID car)
(TOK_ID list)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID more?)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID x)
TOK_RPAREN
TOK_LPAREN
(TOK_ID >=)
(TOK_ID x)
TOK_LPAREN
(TOK_ID car)
(TOK_ID list)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID lesses)
TOK_LPAREN
(TOK_ID filter)
(TOK_ID less?)
TOK_LPAREN
(TOK_ID cdr)
(TOK_ID list)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID mores)
TOK_LPAREN
(TOK_ID filter)
(TOK_ID more?)
TOK_LPAREN
(TOK_ID cdr)
(TOK_ID list)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID ++)
TOK_LPAREN
(TOK_ID quicksort)
(TOK_ID lesses)
TOK_RPAREN
TOK_LPAREN
(TOK_ID cons)
TOK_LPAREN
(TOK_ID car)
(TOK_ID list)
TOK_RPAREN
TOK_LPAREN
(TOK_ID quicksort)
(TOK_ID mores)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID print-list)
TOK_LPAREN
(TOK_ID quicksort)
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 30))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 100))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 12))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 33))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 348))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok -12))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 1239))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 82))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 21))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 193))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 29))
(TOK_ID nil)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID level_one)
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 30))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 100))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 12))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 33))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 348))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok -12))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 1239))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 82))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 21))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 193))
TOK_LPAREN
(TOK_ID cons)
(TOK_INT(Ok 29))
(TOK_ID nil)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID get_level)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID n)
TOK_RPAREN
TOK_LPAREN
(TOK_ID if)
TOK_LPAREN
(TOK_ID =)
(TOK_ID n)
(TOK_INT(Ok 0))
TOK_RPAREN
(TOK_ID level_one)
TOK_LPAREN
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID next_level)
TOK_LPAREN
(TOK_ID get_level)
TOK_LPAREN
(TOK_ID -)
(TOK_ID n)
(TOK_INT(Ok 1))
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID ++)
(TOK_ID next_level)
(TOK_ID next_level)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID not)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID b)
TOK_RPAREN
TOK_LPAREN
(TOK_ID if)
(TOK_ID b)
(TOK_BOOL false)
(TOK_BOOL true)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID or)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID a)
(TOK_ID b)
TOK_RPAREN
TOK_LPAREN
(TOK_ID if)
(TOK_ID a)
(TOK_BOOL true)
(TOK_ID b)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID xor)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID a)
(TOK_ID b)
TOK_RPAREN
TOK_LPAREN
(TOK_ID if)
(TOK_ID a)
TOK_LPAREN
(TOK_ID not)
(TOK_ID b)
TOK_RPAREN
(TOK_ID b)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID and)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID a)
(TOK_ID b)
TOK_RPAREN
TOK_LPAREN
(TOK_ID if)
(TOK_ID a)
(TOK_ID b)
(TOK_BOOL false)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID l=)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID list1)
(TOK_ID list2)
TOK_RPAREN
TOK_LPAREN
(TOK_ID if)
TOK_LPAREN
(TOK_ID xor)
TOK_LPAREN
(TOK_ID nil?)
(TOK_ID list1)
TOK_RPAREN
TOK_LPAREN
(TOK_ID nil?)
(TOK_ID list2)
TOK_RPAREN
TOK_RPAREN
(TOK_BOOL false)
TOK_LPAREN
(TOK_ID if)
TOK_LPAREN
(TOK_ID and)
TOK_LPAREN
(TOK_ID nil?)
(TOK_ID list1)
TOK_RPAREN
TOK_LPAREN
(TOK_ID nil?)
(TOK_ID list2)
TOK_RPAREN
TOK_RPAREN
(TOK_BOOL true)
TOK_LPAREN
(TOK_ID if)
TOK_LPAREN
(TOK_ID =)
TOK_LPAREN
(TOK_ID car)
(TOK_ID list1)
TOK_RPAREN
TOK_LPAREN
(TOK_ID car)
(TOK_ID list2)
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID l=)
TOK_LPAREN
(TOK_ID cdr)
(TOK_ID list1)
TOK_RPAREN
TOK_LPAREN
(TOK_ID cdr)
(TOK_ID list2)
TOK_RPAREN
TOK_RPAREN
(TOK_BOOL false)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID pair)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID a)
(TOK_ID b)
TOK_RPAREN
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID x)
TOK_RPAREN
TOK_LPAREN
(TOK_ID x)
(TOK_ID a)
(TOK_ID b)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID fst)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID t)
TOK_RPAREN
TOK_LPAREN
(TOK_ID t)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID a)
(TOK_ID b)
TOK_RPAREN
(TOK_ID a)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID snd)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID t)
TOK_RPAREN
TOK_LPAREN
(TOK_ID t)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID a)
(TOK_ID b)
TOK_RPAREN
(TOK_ID b)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID print-pair)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID t)
TOK_RPAREN
TOK_LPAREN
(TOK_ID print)
TOK_LPAREN
(TOK_ID fst)
(TOK_ID t)
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID print)
(TOK_CHAR(Ok" "))
TOK_RPAREN
TOK_LPAREN
(TOK_ID print)
TOK_LPAREN
(TOK_ID snd)
(TOK_ID t)
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID print)
(TOK_CHAR(Ok"\n"))
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID print-pair)
TOK_LPAREN
(TOK_ID pair)
(TOK_BOOL true)
(TOK_BOOL true)
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID zip)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID list1)
(TOK_ID list2)
TOK_RPAREN
TOK_LPAREN
(TOK_ID if)
TOK_LPAREN
(TOK_ID or)
TOK_LPAREN
(TOK_ID nil?)
(TOK_ID list1)
TOK_RPAREN
TOK_LPAREN
(TOK_ID nil?)
(TOK_ID list2)
TOK_RPAREN
TOK_RPAREN
(TOK_ID nil)
TOK_LPAREN
(TOK_ID cons)
TOK_LPAREN
(TOK_ID pair)
TOK_LPAREN
(TOK_ID car)
(TOK_ID list1)
TOK_RPAREN
TOK_LPAREN
(TOK_ID car)
(TOK_ID list2)
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID zip)
TOK_LPAREN
(TOK_ID cdr)
(TOK_ID list1)
TOK_RPAREN
TOK_LPAREN
(TOK_ID cdr)
(TOK_ID list2)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID add-pair)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID t)
TOK_RPAREN
TOK_LPAREN
(TOK_ID +)
TOK_LPAREN
(TOK_ID fst)
(TOK_ID t)
TOK_RPAREN
TOK_LPAREN
(TOK_ID snd)
(TOK_ID t)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID add-lists)
TOK_LPAREN
(TOK_ID $$)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID map)
TOK_RPAREN
(TOK_ID add-pair)
TOK_RPAREN
(TOK_ID zip)
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID abs)
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
(TOK_ID x)
TOK_RPAREN
TOK_LPAREN
(TOK_ID if)
TOK_LPAREN
(TOK_ID >=)
(TOK_ID x)
(TOK_INT(Ok 0))
TOK_RPAREN
(TOK_ID x)
TOK_LPAREN
(TOK_ID -)
(TOK_ID x)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID print-endline)
TOK_LPAREN
(TOK_ID =)
(TOK_INT(Ok 0))
TOK_LPAREN
(TOK_ID abs)
(TOK_INT(Ok 0))
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID print-endline)
TOK_LPAREN
(TOK_ID =)
(TOK_INT(Ok 0))
TOK_LPAREN
(TOK_ID abs)
(TOK_INT(Ok 0))
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
TOK_LPAREN
(TOK_ID lambda)
TOK_LPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID print)
(TOK_CHAR(Ok #))
TOK_RPAREN
TOK_LPAREN
(TOK_ID print)
(TOK_CHAR(Ok t))
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID print)
(TOK_CHAR(Ok"\n"))
TOK_RPAREN
TOK_LPAREN
(TOK_ID print-list)
TOK_LPAREN
(TOK_ID cons)
(TOK_CHAR(Ok n))
TOK_LPAREN
(TOK_ID cons)
(TOK_CHAR(Ok e))
TOK_LPAREN
(TOK_ID cons)
(TOK_CHAR(Ok w))
TOK_LPAREN
(TOK_ID cons)
(TOK_CHAR(Ok l))
TOK_LPAREN
(TOK_ID cons)
(TOK_CHAR(Ok i))
TOK_LPAREN
(TOK_ID cons)
(TOK_CHAR(Ok n))
TOK_LPAREN
(TOK_ID cons)
(TOK_CHAR(Ok e))
TOK_LPAREN
(TOK_ID cons)
(TOK_CHAR(Ok"\n"))
(TOK_ID nil)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID a)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok a))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID b)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok b))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID c)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok c))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID d)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok d))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID e)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok e))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID f)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok f))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID g)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok g))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID h)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok h))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID i)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok i))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID j)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok j))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID k)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok k))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID l)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok l))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID m)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok m))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID n)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok n))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID o)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok o))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID p)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok p))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID q)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok q))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID r)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok r))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID s)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok s))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID t)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok t))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID u)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok u))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID v)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok v))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID w)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok w))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID x)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok x))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID y)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok y))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID z)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok z))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID !)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok !))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID space)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok" "))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID define)
(TOK_ID newline)
TOK_LPAREN
TOK_LPAREN
(TOK_ID curry)
(TOK_ID cons)
TOK_RPAREN
(TOK_CHAR(Ok"\n"))
TOK_RPAREN
TOK_RPAREN
TOK_LPAREN
(TOK_ID print-list)
TOK_LPAREN
(TOK_ID h)
TOK_LPAREN
(TOK_ID e)
TOK_LPAREN
(TOK_ID l)
TOK_LPAREN
(TOK_ID l)
TOK_LPAREN
(TOK_ID o)
TOK_LPAREN
(TOK_ID space)
TOK_LPAREN
(TOK_ID w)
TOK_LPAREN
(TOK_ID o)
TOK_LPAREN
(TOK_ID r)
TOK_LPAREN
(TOK_ID l)
TOK_LPAREN
(TOK_ID d)
TOK_LPAREN
(TOK_ID !)
TOK_LPAREN
(TOK_ID newline)
(TOK_ID nil)
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_RPAREN
TOK_EOF
