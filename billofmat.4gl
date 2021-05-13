
CONSTANT root_image = "bomcomp"
CONSTANT node_image = "bomgroup"
CONSTANT leaf_image = "bomleaf"

CONSTANT root_name  = "root"

TYPE t_bompart RECORD
       bompart_code        VARCHAR(10),
       bompart_name        VARCHAR(50),
       bompart_description VARCHAR(200),
       bompart_material    VARCHAR(50),
       bompart_unitprice   DECIMAL(10,2)
     END RECORD

TYPE t_bompl RECORD
       bompart_code        VARCHAR(10),
       bompart_name        VARCHAR(50),
       bompart_description VARCHAR(200),
       bompart_material    VARCHAR(50),
       bompart_unitprice   DECIMAL(10,2),
       bomlink_num         INTEGER,
       bomlink_position    SMALLINT,
       bomlink_quantity    DECIMAL(20,4),
       bomlink_expanded    SMALLINT
     END RECORD

TYPE t_bomtree RECORD
       name                STRING,
       image               STRING,
       pid                 VARCHAR(50),
       id                  VARCHAR(50),
       expanded            BOOLEAN,
       linknum             INTEGER,
       linkpos             SMALLINT,
       description         VARCHAR(200),
       material            VARCHAR(50),
       quantity            DECIMAL(20,4),
       unitprice           DECIMAL(10,2),
       price               DECIMAL(10,2)
     END RECORD

TYPE t_colors RECORD
       name                STRING,
       image               STRING,
       pid                 STRING,
       id                  STRING,
       expanded            STRING,
       linknum             STRING,
       linkpos             STRING,
       description         STRING,
       material            STRING,
       quantity            STRING,
       unitprice           STRING,
       price               STRING
     END RECORD

DEFINE bomlist_colors DYNAMIC ARRAY OF t_colors

DEFINE bomlist DYNAMIC ARRAY OF t_bomtree

DEFINE partlist DYNAMIC ARRAY OF t_bompart

DEFINE options RECORD
       follow_tree SMALLINT,
       part_filter CHAR(1),
       last_row INTEGER
     END RECORD

DEFINE curr_user VARCHAR(50)

MAIN
    DEFINE first BOOLEAN

    OPTIONS INPUT WRAP

    OPEN FORM f1 FROM "billofmat"
    DISPLAY FORM f1

    CONNECT TO ":memory:+driver='dbmsqt'"
    CALL create_tables()

    WHENEVER ERROR CONTINUE
    SELECT DISTINCT USER INTO curr_user FROM bomopts
    IF sqlca.sqlcode != 0 THEN
       LET curr_user = fgl_getenv("LOGNAME")
       IF curr_user IS NULL THEN
          LET curr_user = fgl_getenv("USERNAME")
       END IF
    END IF
    WHENEVER ERROR STOP

    SELECT bomopts_ft, bomopts_pf, bomopts_lr INTO options.*
      FROM bomopts WHERE bomopts_user = curr_user
    IF sqlca.sqlcode == NOTFOUND THEN
       LET options.follow_tree = FALSE
       LET options.part_filter = "A"
       LET options.last_row = NULL
       LET first = TRUE
    END IF

    CALL bom_fill_parts(options.part_filter)
    CALL bom_fill_tree(root_name)
    CALL bom_tree_all_totals()
    CALL bom_build()
    CALL bom_sync_expanded()

    IF first THEN
       INSERT INTO bomopts VALUES (
                   curr_user,
                   options.follow_tree,
                   options.part_filter,
                   options.last_row
              )
    ELSE
       UPDATE bomopts SET
              bomopts_ft = options.follow_tree,
              bomopts_pf = options.part_filter,
              bomopts_lr = options.last_row
        WHERE bomopts_user = curr_user
    END IF

END MAIN

FUNCTION bom_fill_tree(parentid)
    DEFINE parentid VARCHAR(50)
    DEFINE arr DYNAMIC ARRAY OF t_bompl
    DEFINE rec t_bompl
    DEFINE i, j, n, x INT
    DEFINE tot DECIMAL(10,2)

    DECLARE cu1 CURSOR FOR
        SELECT p.*,
               l.bomlink_num, l.bomlink_position,
               l.bomlink_quantity, l.bomlink_expanded
           FROM bomlink l, bompart p
          WHERE l.bomlink_parent = parentid
            AND l.bomlink_component = p.bompart_code
          ORDER BY l.bomlink_position

    LET n = 0
    FOREACH cu1 INTO rec.*
        LET n = n + 1
        LET arr[n].* = rec.*
    END FOREACH

    IF parentid == root_name THEN
       CALL bomlist.clear()
    END IF
    LET x = bomlist.getLength()
    IF n > 0 AND x > 0 THEN 
       IF x == 1 THEN
          LET bomlist[x].image = root_image
          LET bomlist[x].expanded = TRUE
       ELSE
          LET bomlist[x].image = node_image
       END IF
    END IF

    LET tot = 0.0
    FOR i = 1 TO n
        LET j = bomlist.getLength() + 1
        LET bomlist[j].name        = "["||arr[i].bompart_code||"] "||arr[i].bompart_name
        LET bomlist[j].image       = leaf_image
        LET bomlist[j].pid         = parentid
        LET bomlist[j].id          = arr[i].bompart_code
        LET bomlist[j].linknum     = arr[i].bomlink_num
        LET bomlist[j].linkpos     = arr[i].bomlink_position
        LET bomlist[j].description = arr[i].bompart_description
        LET bomlist[j].material    = arr[i].bompart_material
        LET bomlist[j].quantity    = arr[i].bomlink_quantity
        LET bomlist[j].expanded    = arr[i].bomlink_expanded
        LET bomlist[j].unitprice   = arr[i].bompart_unitprice
        LET bomlist[j].price       = arr[i].bompart_unitprice * arr[i].bomlink_quantity
        LET bomlist_colors[j].quantity = "lightblue reverse"
        LET bomlist_colors[j].price = "lightyellow reverse"
        CALL bom_fill_tree(arr[i].bompart_code)
    END FOR

END FUNCTION

FUNCTION bom_tree_all_totals()
    DEFINE i INT
    IF bomlist.getLength()==0 THEN RETURN END IF
    FOR i = 1 TO bomlist.getLength()
       IF bomlist[i].pid == root_name THEN
          CALL bom_tree_parent_total(1)
       END IF
    END FOR
END FUNCTION

FUNCTION bom_fill_parts(filter)
    DEFINE filter STRING
    DEFINE rec t_bompart
    DEFINE i INT
    DEFINE sql STRING

    CASE options.part_filter
         WHEN "A" LET filter = NULL
         WHEN "M" LET filter = "bompart_material != 'Type'"
         WHEN "C" LET filter = "bompart_material = 'Type'"
    END CASE

    LET sql = "SELECT p.* FROM bompart p WHERE bompart_code != 'root'"
    IF filter IS NOT NULL THEN
       LET sql = sql || " AND " || filter
    END IF
    LET sql = sql || " ORDER BY p.bompart_name"
    DECLARE cu2 CURSOR FROM sql

    CALL partlist.clear()
    LET i = 1
    FOREACH cu2 INTO rec.*
        LET partlist[i].* = rec.*
        LET i = i + 1
    END FOREACH

END FUNCTION

FUNCTION bom_parts_lookup(code)
    DEFINE code STRING
    DEFINE i INT
    FOR i=1 TO partlist.getLength()
        IF partlist[i].bompart_code = code THEN RETURN i END IF
    END FOR
    RETURN 0
END FUNCTION

FUNCTION bom_dialog_setup(d)
    DEFINE d ui.Dialog
    DEFINE n, c, x INT

    LET n = d.getArrayLength("tvsr")
    LET c = d.getCurrentRow("tvsr")
    IF options.follow_tree AND n > 0 THEN
       LET x = bom_parts_lookup(bomlist[c].id)
       IF x > 0 THEN
          CALL d.setCurrentRow("mlsr", x)
       END IF
    END IF

    IF c <= 0 THEN
       CALL d.setActionActive("tree_delete", FALSE)
       CALL d.setActionActive("tree_append_child", FALSE)
       CALL d.setActionActive("tree_qincr", FALSE)
       CALL d.setActionActive("tree_qdecr", FALSE)
       CALL d.setActionActive("tree_mup",  FALSE)
       CALL d.setActionActive("tree_mdown", FALSE)
       CALL d.setActionActive("tree_expall", FALSE)
       CALL d.setActionActive("tree_colall", FALSE)
       CALL d.setActionActive("tree_clear", FALSE)
    ELSE
       CALL d.setActionActive("tree_delete", bom_can_delete(d, -1))
       CALL d.setActionActive("tree_append_child", bom_can_append_child(d))
       CALL d.setActionActive("tree_qincr", NOT bom_tree_is_parent(d.getCurrentRow("tvsr")))
       CALL d.setActionActive("tree_qdecr", NOT bom_tree_is_parent(d.getCurrentRow("tvsr")) AND bomlist[c].quantity > 1)
       CALL d.setActionActive("tree_mup", bom_tree_can_up(d,c))
       CALL d.setActionActive("tree_mdown", bom_tree_can_down(d,c))
       CALL d.setActionActive("tree_expall", bom_tree_is_parent(c) AND NOT bomlist[c].expanded)
       CALL d.setActionActive("tree_colall", bom_tree_is_parent(c) AND bomlist[c].expanded)
       CALL d.setActionActive("tree_clear", TRUE)
    END IF
    CALL d.setActionActive("tree_insert_sibling", bom_can_insert_sibling(d))

END FUNCTION

FUNCTION bom_tree_is_parent(c)
    DEFINE c INT
    IF c <= 0 THEN RETURN FALSE END IF
    IF c == bomlist.getLength() THEN RETURN FALSE END IF
    RETURN ( bomlist[c+1].pid == bomlist[c].id )
END FUNCTION

FUNCTION bom_can_delete(d, c)
    DEFINE d ui.Dialog
    DEFINE c INT
    IF c == -1 THEN
       LET c = d.getCurrentRow("tvsr")
    END IF
    IF c <= 0 THEN RETURN FALSE END IF
    RETURN NOT bom_tree_is_parent(c)
END FUNCTION

FUNCTION bom_init_node(c, pr, pid, num, pos)
    DEFINE c, pr, num, pos INT
    DEFINE pid STRING
    LET bomlist[c].name        = "["||partlist[pr].bompart_code||"] "||partlist[pr].bompart_name
    LET bomlist[c].image       = leaf_image
    LET bomlist[c].pid         = pid
    LET bomlist[c].id          = partlist[pr].bompart_code
    LET bomlist[c].linknum     = num
    LET bomlist[c].linkpos     = pos
    LET bomlist[c].description = partlist[pr].bompart_description
    LET bomlist[c].material    = partlist[pr].bompart_material
    IF partlist[pr].bompart_material=="Type" THEN
        LET bomlist[c].quantity    = NULL
    ELSE
        LET bomlist[c].quantity    = 1
    END IF
    LET bomlist[c].unitprice   = partlist[pr].bompart_unitprice
    LET bomlist[c].price       = partlist[pr].bompart_unitprice
    LET bomlist_colors[c].quantity = "lightblue reverse"
    LET bomlist_colors[c].price    = "lightyellow reverse"
END FUNCTION

FUNCTION bom_positions_update(c, v)
    DEFINE c, v, i, s, e INT
    DEFINE arr DYNAMIC ARRAY OF INTEGER
    FOR i=c TO bomlist.getLength()
        IF bomlist[i].pid == bomlist[c].pid THEN
           CALL arr.appendElement()
           LET arr[arr.getLength()] = i
           LET bomlist[i].linkpos = bomlist[i].linkpos + v
        END IF
    END FOR
    IF v == 1 THEN
        LET s = arr.getLength()
        LET e = 1
    ELSE
        LET s = 1
        LET e = arr.getLength()
    END IF
    FOR i=s TO e STEP -v
        UPDATE bomlink SET bomlink_position = bomlist[arr[i]].linkpos
         WHERE bomlink_num = bomlist[arr[i]].linknum
    END FOR
END FUNCTION

FUNCTION bom_can_append_child(d)
    DEFINE d ui.Dialog
    DEFINE c, pr, i INT
    LET pr = d.getCurrentRow("mlsr")
    LET c = d.getCurrentRow("tvsr")
    IF NOT bom_can_insert_part(d,c,pr) THEN RETURN FALSE END IF
    IF bomlist[c].material != "Type" THEN RETURN FALSE END IF
    FOR i=c+1 TO bomlist.getLength()
        IF bomlist[i].pid != bomlist[c].id THEN EXIT FOR END IF
        IF bomlist[i].id == partlist[pr].bompart_code THEN RETURN FALSE END IF
    END FOR
    RETURN TRUE
END FUNCTION

FUNCTION bom_count_children(p)
    DEFINE p, i, c INT
    LET c = 0
    FOR i=p+1 TO bomlist.getLength()
        IF bomlist[i].pid == bomlist[p].id THEN LET c=c+1 END IF
    END FOR
    RETURN c
END FUNCTION

FUNCTION bom_append_child(d, pr)
    DEFINE d ui.Dialog
    DEFINE pr, p, c, x INT
    LET p = d.getCurrentRow("tvsr")
    LET x = bom_count_children(p)
    LET bomlist[p].image     = node_image
    LET bomlist[p].expanded  = TRUE
    LET c = d.appendNode("tvsr", p)
    CALL bom_init_node(c, pr, bomlist[c].pid, 0, x+1)
    LET bomlist[c].linknum = bomlink_new_id()
    INSERT INTO bomlink VALUES
         (
           bomlist[c].linknum,
           bomlist[c].pid,
           bomlist[c].id,
           bomlist[c].linkpos,
           bomlist[c].quantity,
           0
         )
    MESSAGE "APPEND CHILD: ", bomlist[c].pid, " / ", bomlist[c].id
    CALL bom_tree_parent_total(bom_tree_root(c))
    RETURN TRUE
END FUNCTION

FUNCTION bom_tree_parent_index(c)
    DEFINE c,i INT
    DEFINE pid STRING
    LET pid = bomlist[c].pid
    FOR i=c TO 1 STEP -1
        IF bomlist[i].pid != pid THEN RETURN i END IF
    END FOR
    RETURN 0
END FUNCTION

FUNCTION bom_can_insert_part(d, c, pr)
    DEFINE d ui.Dialog
    DEFINE c, pr, i INT
    LET d = NULL
    IF pr <= 0 THEN RETURN FALSE END IF
    IF c < 0 THEN RETURN FALSE END IF
    IF partlist[pr].bompart_material == "Type" THEN
       FOR i=1 TO bomlist.getLength()
           IF bomlist[i].id == partlist[pr].bompart_code THEN RETURN FALSE END IF
       END FOR
    END IF
    RETURN TRUE
END FUNCTION

FUNCTION bom_can_insert_sibling(d)
    DEFINE d ui.Dialog
    DEFINE c, pr, i, x INT
    LET pr = d.getCurrentRow("mlsr")
    LET c = d.getCurrentRow("tvsr")
    IF NOT bom_can_insert_part(d,c,pr) THEN RETURN FALSE END IF
    IF c == 0 THEN RETURN (partlist[pr].bompart_material == 'Type') END IF
    IF c == 1 THEN RETURN FALSE END IF
    IF bomlist[c].pid == 'root' AND partlist[pr].bompart_material != 'Type' THEN RETURN FALSE END IF
    LET x = bom_tree_parent_index(c)
    IF x>0 THEN
       FOR i=x+1 TO bomlist.getLength()
           IF bomlist[i].pid != bomlist[c].pid THEN EXIT FOR END IF
           IF bomlist[i].id == partlist[pr].bompart_code THEN RETURN FALSE END IF
       END FOR
    ELSE
       IF bomlist[1].id == partlist[pr].bompart_code THEN RETURN FALSE END IF
    END IF
    RETURN TRUE
END FUNCTION

FUNCTION bom_insert_sibling(d, pr)
    DEFINE d ui.Dialog
    DEFINE pr, c, x INT
    LET c = d.getCurrentRow("tvsr")
    LET x = bom_tree_check_loop(partlist[pr].bompart_code, c)
    IF x > 0 THEN
       CALL __mbox_ok("Insert sibling",
                      SFMT("Inserting part %1 would result in a loop (used in row %2)", partlist[pr].bompart_code, x),
                      "stop")
       RETURN FALSE
    END IF
    IF c <= 0 THEN
       LET c = d.appendNode("tvsr", 0)
       CALL bom_init_node(c, pr, root_name, 0, 1)
       CALL d.setCurrentRow("tvsr", c)
    ELSE
       CALL bom_positions_update(c, +1)
       CALL d.insertNode("tvsr", c)
       CALL bom_init_node(c, pr, bomlist[c+1].pid, 0, bomlist[c+1].linkpos-1)
    END IF
    LET bomlist[c].linknum = bomlink_new_id()
    INSERT INTO bomlink VALUES
         (
           bomlist[c].linknum,
           bomlist[c].pid,
           bomlist[c].id,
           bomlist[c].linkpos,
           bomlist[c].quantity,
           0
         )
    MESSAGE "INSERT SIBLING: ", bomlist[c].pid, " / ", bomlist[c].id
    CALL bom_tree_parent_total(bom_tree_root(c))
    RETURN TRUE
END FUNCTION

FUNCTION bom_debug()
    DEFINE i INT
    DISPLAY "------------------------------------------------------------"
    FOR i=1 TO bomlist.getLength()
        DISPLAY bomlist[i].pid, "/", bomlist[i].id
    END FOR
END FUNCTION

FUNCTION bom_tree_delete(d)
    DEFINE d ui.Dialog
    DEFINE c, x, p INT
    DEFINE pid STRING
    LET c = d.getCurrentRow("tvsr")
    LET pid = bomlist[c].pid
    DELETE FROM bomlink WHERE bomlink_num = bomlist[c].linknum
    CALL bom_positions_update(c, -1)
    CALL d.deleteNode("tvsr", c)
    IF c > 1 THEN
       LET p = c-1
       IF bomlist[p].id = pid THEN
          IF NOT bom_tree_is_parent(p) THEN
             LET bomlist[p].image = leaf_image
             LET bomlist[p].price = NULL
          END IF
          LET x = bom_tree_root(c)
          IF x == 0 THEN LET x = bom_tree_root(p) END IF
          CALL bom_tree_parent_total(x)
       ELSE
          CALL bom_tree_parent_total(bom_tree_root(p))
       END IF
    END IF
END FUNCTION

FUNCTION bom_tree_check_loop(id_to_check, c)
    DEFINE id_to_check STRING, c INT
    DEFINE i, x INT
    IF c == 0 THEN RETURN 0 END IF
    LET i = c-1
    WHILE i>=1
        IF bomlist[i].pid != bomlist[c].pid THEN
           EXIT WHILE
        END IF
        LET i=i-1
    END WHILE
    IF i==0 THEN RETURN 0 END IF
    IF bomlist[i].pid = id_to_check THEN RETURN i END IF
    LET x = bom_tree_check_loop(id_to_check, i)
    IF x > 0 THEN RETURN x END IF
    RETURN 0
END FUNCTION

FUNCTION bom_tree_root(c)
    DEFINE c, i INTEGER
    IF c == 0 OR c>bomlist.getLength() THEN RETURN 0 END IF
    LET i = c-1
    WHILE i>=1
        IF bomlist[i].id == bomlist[c].pid THEN
           EXIT WHILE
        END IF
        LET i=i-1
    END WHILE
    IF i==0 THEN RETURN 0 END IF
    IF bomlist[i].pid == root_name THEN RETURN i END IF
    RETURN bom_tree_root(i)
END FUNCTION

FUNCTION bom_tree_clear(d)
    DEFINE d ui.Dialog
    IF NOT __mbox_yn("Clear",
           "Are you sure you want to delete all nodes of the structure?",
           "exclamation") THEN RETURN END IF
    DELETE FROM bomlink
    CALL d.deleteAllRows("tvsr")
END FUNCTION

FUNCTION bom_tree_can_up(d, c)
    DEFINE d ui.Dialog
    DEFINE c INT
    LET d = NULL
    IF bom_tree_is_parent(c) THEN RETURN FALSE END IF
    IF c <= 2 THEN RETURN FALSE END IF
    IF bomlist[c-1].pid != bomlist[c].pid THEN RETURN FALSE END IF
    IF bom_tree_is_parent(c-1) THEN RETURN FALSE END IF
    RETURN TRUE
END FUNCTION

FUNCTION bom_tree_up(d, c)
    DEFINE d ui.Dialog
    DEFINE c, x INT
    BEGIN WORK
    LET x = bomlist[c].linkpos
    UPDATE bomlink SET bomlink_position = -1  WHERE bomlink_num = bomlist[c-1].linknum
    UPDATE bomlink SET bomlink_position = x-1 WHERE bomlink_num = bomlist[c  ].linknum
    UPDATE bomlink SET bomlink_position = x   WHERE bomlink_num = bomlist[c-1].linknum
    COMMIT WORK
    CALL d.insertNode("tvsr", c)
    CALL d.insertNode("tvsr", c)
    LET bomlist[c].* = bomlist[c+2].*
    LET bomlist[c].linkpos = x-1
    LET bomlist_colors[c].* = bomlist_colors[c+2].*
    LET bomlist[c+1].* = bomlist[c-1].*
    LET bomlist[c+1].linkpos = x
    LET bomlist_colors[c+1].* = bomlist_colors[c-1].*
    CALL d.deleteNode("tvsr", c+2)
    CALL d.deleteNode("tvsr", c-1)
    CALL d.setCurrentRow("tvsr", c-1)
END FUNCTION

FUNCTION bom_tree_can_down(d, c)
    DEFINE d ui.Dialog
    DEFINE c INT
    IF c == 0 THEN RETURN FALSE END IF
    IF bom_tree_is_parent(c) THEN RETURN FALSE END IF
    IF c == d.getArrayLength("tvsr") THEN RETURN FALSE END IF
    IF bomlist[c+1].pid != bomlist[c].pid THEN RETURN FALSE END IF
    IF bom_tree_is_parent(c+1) THEN RETURN FALSE END IF
    RETURN TRUE
END FUNCTION

FUNCTION bom_tree_down(d, c)
    DEFINE d ui.Dialog
    DEFINE c, x INT
    BEGIN WORK
    LET x = bomlist[c].linkpos
    UPDATE bomlink SET bomlink_position = -1  WHERE bomlink_num = bomlist[c+1].linknum
    UPDATE bomlink SET bomlink_position = x+1 WHERE bomlink_num = bomlist[c  ].linknum
    UPDATE bomlink SET bomlink_position = x   WHERE bomlink_num = bomlist[c+1].linknum
    COMMIT WORK
    CALL d.insertNode("tvsr", c+1)
    CALL d.insertNode("tvsr", c+1)
    LET bomlist[c+1].* = bomlist[c+3].*
    LET bomlist[c+1].linkpos = x
    LET bomlist_colors[c+1].* = bomlist_colors[c+3].*
    LET bomlist[c+2].* = bomlist[c].*
    LET bomlist[c+2].linkpos = x+1
    LET bomlist_colors[c+2].* = bomlist_colors[c].*
    CALL d.deleteNode("tvsr", c+3)
    CALL d.deleteNode("tvsr", c)
    CALL d.setCurrentRow("tvsr", c+1)
    CALL d.setCurrentRow("tvsr", c+1)
END FUNCTION

FUNCTION bom_tree_parent_total(c)
    DEFINE c, i INT
    DEFINE total DECIMAL(10,2)
    LET total = 0.00
    IF c<=0 OR c>bomlist.getLength() THEN RETURN END IF
    FOR i=c+1 TO bomlist.getLength()
        IF bomlist[i].pid == bomlist[c].id THEN
           IF bom_tree_is_parent(i) THEN
              CALL bom_tree_parent_total(i)
           END IF
           LET total = total + bomlist[i].price
        END IF
    END FOR
    IF total IS NULL AND bomlist[c].price IS NOT NULL
    OR total IS NOT NULL AND bomlist[c].price IS NULL
    OR total != bomlist[c].price THEN
       LET bomlist_colors[c].price = "red reverse"
    END IF
    LET bomlist[c].price = total
END FUNCTION

FUNCTION bom_tree_quantity_add(d, c, v)
    DEFINE d ui.Dialog
    DEFINE c INT
    DEFINE v DECIMAL(20,4)
    LET d = NULL
    UPDATE bomlink SET bomlink_quantity = bomlist[c].quantity + v
     WHERE bomlink_num = bomlist[c].linknum
    LET bomlist[c].quantity = bomlist[c].quantity + v
    LET bomlist[c].price = bomlist[c].quantity * bomlist[c].unitprice
    CALL bom_tree_parent_total(bom_tree_root(c))
END FUNCTION

FUNCTION bom_tree_expcolall(d, c, v)
    DEFINE d ui.Dialog
    DEFINE c, v, i INT
    LET d = NULL
    FOR i=c TO bomlist.getLength()
        IF i!=c AND bomlist[i].pid == bomlist[c].pid THEN EXIT FOR END IF
        LET bomlist[i].expanded = v
    END FOR
END FUNCTION

FUNCTION bom_sync_expanded()
    DEFINE i, x INT
    BEGIN WORK
    FOR i=1 TO bomlist.getLength()
        IF bomlist[i].expanded THEN LET x = 1 ELSE LET x = 0 END IF
        UPDATE bomlink SET bomlink_expanded = x
         WHERE bomlink_num = bomlist[i].linknum
    END FOR
    COMMIT WORK
END FUNCTION

FUNCTION bom_build()
    DEFINE x INT

    DIALOG ATTRIBUTES(UNBUFFERED)

        DISPLAY ARRAY bomlist TO tvsr.*
            BEFORE ROW
               CALL bom_dialog_setup(DIALOG)
            AFTER ROW
               FOR x=1 TO DIALOG.getArrayLength("tvsr")
                   LET bomlist_colors[x].price = "lightyellow reverse"
               END FOR
        END DISPLAY

        DISPLAY ARRAY partlist TO mlsr.*
            BEFORE ROW
               CALL bom_dialog_setup(DIALOG)
        END DISPLAY

        INPUT BY NAME options.part_filter, options.follow_tree
              ATTRIBUTES(WITHOUT DEFAULTS)
           ON CHANGE follow_tree
              CALL bom_dialog_setup(DIALOG)
           ON CHANGE part_filter
              CALL bom_fill_parts(options.part_filter)
              CALL DIALOG.setCurrentRow("mlsr",1)
              CALL bom_dialog_setup(DIALOG)
        END INPUT

        BEFORE DIALOG
           CALL DIALOG.setCurrentRow("tvsr", options.last_row)
           CALL DIALOG.setArrayAttributes("tvsr", bomlist_colors)
           CALL bom_dialog_setup(DIALOG) -- When no rows in tree.
        AFTER DIALOG
           LET options.last_row = DIALOG.getCurrentRow("tvsr")

        ON ACTION tree_delete
           CALL bom_tree_delete(DIALOG)
           CALL bom_dialog_setup(DIALOG)
        ON ACTION tree_qincr
           CALL bom_tree_quantity_add(DIALOG, DIALOG.getCurrentRow("tvsr"), 1)
           CALL bom_dialog_setup(DIALOG)
        ON ACTION tree_qdecr
           CALL bom_tree_quantity_add(DIALOG, DIALOG.getCurrentRow("tvsr"), -1)
           CALL bom_dialog_setup(DIALOG)
        ON ACTION tree_expall
           CALL bom_tree_expcolall(DIALOG, DIALOG.getCurrentRow("tvsr"), TRUE)
           CALL bom_dialog_setup(DIALOG)
        ON ACTION tree_colall
           CALL bom_tree_expcolall(DIALOG, DIALOG.getCurrentRow("tvsr"), FALSE)
           CALL bom_dialog_setup(DIALOG)
        ON ACTION tree_clear
           CALL bom_tree_clear(DIALOG)
           CALL bom_dialog_setup(DIALOG)
        ON ACTION tree_mup
           CALL bom_tree_up(DIALOG, DIALOG.getCurrentRow("tvsr"))
           CALL bom_dialog_setup(DIALOG)
        ON ACTION tree_mdown
           CALL bom_tree_down(DIALOG, DIALOG.getCurrentRow("tvsr"))
           CALL bom_dialog_setup(DIALOG)
        ON ACTION tree_append_child
           IF NOT bom_append_child(DIALOG, DIALOG.getCurrentRow("mlsr")) THEN
              ERROR "Could not insert part at this position"
           END IF
           CALL bom_dialog_setup(DIALOG)
        ON ACTION tree_insert_sibling
           IF NOT bom_insert_sibling(DIALOG, DIALOG.getCurrentRow("mlsr")) THEN
              ERROR "Could not insert sibling at this position"
           END IF
           CALL DIALOG.setCurrentRow("tvsr", DIALOG.getCurrentRow("tvsr")-1)
           CALL bom_dialog_setup(DIALOG)
        ON ACTION show_part
           LET x = bom_parts_lookup(bomlist[DIALOG.getCurrentRow("tvsr")].id)
           IF x > 0 THEN
              CALL DIALOG.setCurrentRow("mlsr", x)
           END IF
 
        ON ACTION bom_debug ATTRIBUTES(DEFAULTVIEW=NO, ACCELERATOR="CONTROL-Z")
           CALL bom_debug()

        ON ACTION close
           ACCEPT DIALOG

    END DIALOG

END FUNCTION

FUNCTION add_constraint(tabname, conname, conbody)
    DEFINE tabname, conname, conbody STRING
    IF fgl_db_driver_type() == "sqt" THEN
       DISPLAY SFMT("%1/%2: No table constaints with SQLite...",tabname,conname)
       RETURN TRUE
    END IF
    WHENEVER ERROR CONTINUE
    EXECUTE IMMEDIATE "ALTER TABLE "||tabname||" ADD CONSTRAINT "||conname||" "||conbody
    IF sqlca.sqlcode != 0 THEN -- Try the Informix syntax
       EXECUTE IMMEDIATE "ALTER TABLE "||tabname||" ADD CONSTRAINT "||conbody||" CONSTRAINT "||conname
    END IF
    WHENEVER ERROR STOP
    RETURN (sqlca.sqlcode == 0)
END FUNCTION

FUNCTION bomlink_new_id()
    DEFINE n INTEGER
    SELECT MAX(bomlink_num)+1 INTO n FROM bomlink
    IF n IS NULL THEN LET n = 1 END IF
    RETURN n
END FUNCTION

FUNCTION create_tables()
    DEFINE s BOOLEAN

    DISPLAY "Creating tables..."
    WHENEVER ERROR CONTINUE
    DROP TABLE bomopts
    DROP TABLE bomlink
    DROP TABLE bompart
    WHENEVER ERROR STOP

    DISPLAY " table bomopts"
    CREATE TABLE bomopts(
                     bomopts_user        VARCHAR(50) NOT NULL PRIMARY KEY,
                     bomopts_ft          SMALLINT,
                     bomopts_pf          CHAR(1),
                     bomopts_lr          INTEGER
                 )

    DISPLAY " table bompart"
    CREATE TABLE bompart (
                     bompart_code        VARCHAR(10) NOT NULL PRIMARY KEY,
                     bompart_name        VARCHAR(50) NOT NULL,
                     bompart_description VARCHAR(200),
                     bompart_material    VARCHAR(50) NOT NULL,
                     bompart_unitprice   DECIMAL(10,2)
                 )
    LET s = add_constraint("bompart", "bompart_c1", "UNIQUE (bompart_name)")
    IF NOT s THEN
       DISPLAY "Warning: Could not create unique constraint on bompart table..."
    END IF

    INSERT INTO bompart VALUES ('root'   ,'Root node'           ,NULL                ,'Type'      ,  NULL )

    INSERT INTO bompart VALUES ('BI1'   ,'Bicycle type 1'       ,NULL                ,'Type'      ,  NULL )
    INSERT INTO bompart VALUES ('BI2'   ,'Bicycle type 2'       ,NULL                ,'Type'      ,  NULL )
    INSERT INTO bompart VALUES ('BI3'   ,'Bicycle type 3'       ,NULL                ,'Type'      ,  NULL )

    INSERT INTO bompart VALUES ('FR1'   ,'Frame type 1'         ,NULL                ,'Type'      ,  NULL )
    INSERT INTO bompart VALUES ('FR2'   ,'Frame type 2'         ,NULL                ,'Type'      ,  NULL )
    INSERT INTO bompart VALUES ('FR3'   ,'Frame type 3'         ,NULL                ,'Type'      ,  NULL )
    INSERT INTO bompart VALUES ('FB101' ,'Frame base (18")'     ,'16" frame base'    ,'Aluminium' , 15.30 )
    INSERT INTO bompart VALUES ('FB102' ,'Frame base (20")'     ,'20" frame base'    ,'Aluminium' , 20.30 )
    INSERT INTO bompart VALUES ('FB103' ,'Frame base (23")'     ,'23" frame base'    ,'Aluminium' , 25.30 )
    INSERT INTO bompart VALUES ('HA101' ,'Handle bar (15")'     ,'15" handle bar'    ,'Aluminium' ,  5.50 )
    INSERT INTO bompart VALUES ('HA102' ,'Handle bar (16")'     ,'16" handle bar'    ,'Aluminium' ,  6.50 )
    INSERT INTO bompart VALUES ('HA103' ,'Handle bar (17")'     ,'17" handle bar'    ,'Aluminium' ,  7.50 )
    INSERT INTO bompart VALUES ('SA101' ,'Saddle (8")'          ,'8" regular saddle' ,'Mixed'     ,  8.50 )
    INSERT INTO bompart VALUES ('SA102' ,'Saddle (10")'         ,'10" regular saddle','Mixed'     ,  9.50 )
    INSERT INTO bompart VALUES ('SC101' ,'Saddle/Sport (8")'    ,'8" sport saddle'   ,'Mixed'     , 10.50 )
    INSERT INTO bompart VALUES ('SC102' ,'Saddle/Sport (10")'   ,'10" sport saddle'  ,'Mixed'     , 12.50 )

    INSERT INTO bompart VALUES ('FW1'   ,'Front wheel type 1'   ,NULL                ,'Type'      ,  NULL )
    INSERT INTO bompart VALUES ('BW1'   ,'Back wheel type 1'    ,NULL                ,'Type'      ,  NULL )
    INSERT INTO bompart VALUES ('TR101' ,'Tire/20'              ,'20" tires'         ,'Rubber'    ,  3.45 )
    INSERT INTO bompart VALUES ('TR102' ,'Tire/26'              ,'26" tires'         ,'Rubber'    ,  8.45 )
    INSERT INTO bompart VALUES ('TR103' ,'Tire/27'              ,'27" tires'         ,'Rubber'    ,  8.45 )
    INSERT INTO bompart VALUES ('RM101' ,'Rim/20 (standard)'    ,'20" standard rim'  ,'Steel'     ,  8.25 )
    INSERT INTO bompart VALUES ('RM102' ,'Rim/26 (standard)'    ,'26" standard rim'  ,'Steel'     ,  8.25 )
    INSERT INTO bompart VALUES ('RM103' ,'Rim/27 (standard)'    ,'27" standard rim'  ,'Steel'     ,  8.25 )
    INSERT INTO bompart VALUES ('RS101' ,'SportRim/20'          ,'20" sport rim'     ,'Graphite'  , 14.45 )
    INSERT INTO bompart VALUES ('RS102' ,'SportRim/26'          ,'26" sport rim'     ,'Graphite'  , 15.45 )
    INSERT INTO bompart VALUES ('RS103' ,'SportRim/27'          ,'27" sport rim'     ,'Graphite'  , 16.45 )
    INSERT INTO bompart VALUES ('FH1'   ,'Front hub type 1'     ,NULL                ,'Type'      ,  NULL )
    INSERT INTO bompart VALUES ('BH1'   ,'Back hub type 1'      ,NULL                ,'Type'      ,  NULL )
    INSERT INTO bompart VALUES ('HS101' ,'Hub shell (regular)'  ,'Regular hub'       ,'Mixed'     , 12.95 )
    INSERT INTO bompart VALUES ('HS102' ,'Hub shell (sport)'    ,'Competition hub'   ,'Mixed'     , 19.20 )
    INSERT INTO bompart VALUES ('AX101' ,'Axle (standard)'      ,'Standard axle'     ,'Steel'     ,  2.40 )
    INSERT INTO bompart VALUES ('AX102' ,'Axle (sport)'         ,'Competition axle'  ,'Steel'     ,  5.25 )
    INSERT INTO bompart VALUES ('HG101' ,'Hub gear S1'          ,'S1 gear'           ,'Steel'     ,  2.25 )
    INSERT INTO bompart VALUES ('HG102' ,'Hub gear S2'          ,'S2 gear'           ,'Steel'     ,  2.35 )
    INSERT INTO bompart VALUES ('HG103' ,'Hub gear S3'          ,'S3 gear'           ,'Steel'     ,  2.45 )
    INSERT INTO bompart VALUES ('HG104' ,'Hub gear S4'          ,'S4 gear'           ,'Steel'     ,  2.55 )

    INSERT INTO bompart VALUES ('GS1'   ,'Gear system type 1'   ,NULL                ,'Type'      ,  NULL )
    INSERT INTO bompart VALUES ('GS2'   ,'Gear system type 2'   ,NULL                ,'Type'      ,  NULL )
    INSERT INTO bompart VALUES ('SF101' ,'Shifter (standard)'   ,'Standard shifter'  ,'Mixed'     ,  5.35 )
    INSERT INTO bompart VALUES ('SF102' ,'Shifter (sport)'      ,'Sport shifter'     ,'Mixed'     , 13.35 )
    INSERT INTO bompart VALUES ('CR101' ,'Chain ring (small)'   ,'6" chain ring'     ,'Steel'     ,  3.35 )
    INSERT INTO bompart VALUES ('CR102' ,'Chain ring (medium)'  ,'7" chain ring'     ,'Steel'     ,  4.35 )
    INSERT INTO bompart VALUES ('CR103' ,'Chain ring (large)'   ,'8" chain ring'     ,'Steel'     ,  5.35 )
    INSERT INTO bompart VALUES ('CH101' ,'Chain (regular)'      ,'Regular chain'     ,'Steel'     ,  3.35 )
    INSERT INTO bompart VALUES ('CH102' ,'Chain (sport)'        ,'Competition chain' ,'Mixed'     ,  8.25 )
    INSERT INTO bompart VALUES ('PE101' ,'Pedal/3'              ,'3" pedals'         ,'Steel'     ,  4.35 )
    INSERT INTO bompart VALUES ('PE102' ,'Pedal/4'              ,'4" pedals'         ,'Steel'     ,  5.35 )

    INSERT INTO bompart VALUES ('BK1'   ,'Brakes type 1'        ,NULL                ,'Type'      ,  NULL )
    INSERT INTO bompart VALUES ('BK2'   ,'Brakes type 2'        ,NULL                ,'Type'      ,  NULL )
    INSERT INTO bompart VALUES ('BC101' ,'Brake cable'          ,'Brake cable'       ,'Mixed'     ,  2.20 )
    INSERT INTO bompart VALUES ('BP101' ,'Brake pad'           ,'Brake pad'          ,'Steel'     ,  4.20 )
    INSERT INTO bompart VALUES ('BD101' ,'Brake disk (small)'   ,'5" brake disk'     ,'Steel'     ,  3.35 )
    INSERT INTO bompart VALUES ('BD102' ,'Brake disk (medium)'  ,'6" brake disk'     ,'Steel'     ,  4.35 )
    INSERT INTO bompart VALUES ('BD103' ,'Brake disk (large)'   ,'7" brake disk'     ,'Steel'     ,  5.35 )

    DISPLAY " table bomlink"
    CREATE TABLE bomlink (
                     bomlink_num        INTEGER PRIMARY KEY,
                     bomlink_parent     VARCHAR(10) NOT NULL,
                     bomlink_component  VARCHAR(10) NOT NULL,
                     bomlink_position   SMALLINT NOT NULL,
                     bomlink_quantity   DECIMAL(20,4),
                     bomlink_expanded   SMALLINT NOT NULL
                 )
    LET s = add_constraint("bomlink", "bomlink_c1", "UNIQUE (bomlink_parent, bomlink_component)")
    IF NOT s THEN
       DISPLAY "Warning: Could not create unique constraint #1 on bompart table..."
    END IF
    LET s = add_constraint("bomlink", "bomlink_c2", "UNIQUE (bomlink_parent, bomlink_position)")
    IF NOT s THEN
       DISPLAY "Warning: Could not create unique constraint #2 on bompart table..."
    END IF
    LET s = add_constraint("bomlink", "bomlink_c3", "FOREIGN KEY (bomlink_parent) REFERENCES bompart (bompart_code)")
    IF NOT s THEN
       DISPLAY "Warning: Could not create foreign key constraint #1 on bompart table..."
    END IF
    LET s = add_constraint("bomlink", "bomlink_c4", "FOREIGN KEY (bomlink_component) REFERENCES bompart (bompart_code)")
    IF NOT s THEN
       DISPLAY "Warning: Could not create foreign key constraint #2 on bompart table..."
    END IF

    INSERT INTO bomlink VALUES (0, 'root',  'BI1',      1, NULL , 1)

    INSERT INTO bomlink VALUES (1, 'BI1',    'FR1',     1, NULL , 1)
              INSERT INTO bomlink VALUES (11, 'FR1',  'FB103',   1, 1.0, 0 )
              INSERT INTO bomlink VALUES (12, 'FR1',  'HA103',   2, 1.0, 0 )
              INSERT INTO bomlink VALUES (13, 'FR1',  'SA102',   3, 1.0, 0 )

    INSERT INTO bomlink VALUES (2, 'BI1',   'FW1',     2, NULL, 0 )
             INSERT INTO bomlink VALUES (21, 'FW1',   'TR103',   1,  1.0, 0 )
             INSERT INTO bomlink VALUES (22, 'FW1',   'RM103',   2,  1.0, 0 )
             INSERT INTO bomlink VALUES (23, 'FW1',   'BH1',     3, NULL, 0 )
                      INSERT INTO bomlink VALUES (231, 'BH1',    'HS102',   1, 1.0, 0 )
                      INSERT INTO bomlink VALUES (232, 'BH1',    'AX102',   2, 1.0, 0 )
                      INSERT INTO bomlink VALUES (233, 'BH1',    'HG104',   3, 1.0, 0 )

    INSERT INTO bomlink VALUES (3, 'BI1',   'BW1',     3, NULL, 0 )
             INSERT INTO bomlink VALUES (31, 'BW1',   'TR103',   1,  1.0, 0 )
             INSERT INTO bomlink VALUES (32, 'BW1',   'RM103',   2,  1.0, 0 )
             INSERT INTO bomlink VALUES (33, 'BW1',   'FH1',     3, NULL, 0 )
                      INSERT INTO bomlink VALUES (331, 'FH1',    'HS102',   1, 1.0, 0 )
                      INSERT INTO bomlink VALUES (332, 'FH1',    'AX102',   2, 1.0, 0 )

    INSERT INTO bomlink VALUES (4, 'BI1',   'GS1',     4, NULL, 0 )
             INSERT INTO bomlink VALUES (41, 'GS1',   'SF102',   1, 1.0, 0 )
             INSERT INTO bomlink VALUES (42, 'GS1',   'CR102',   2, 1.0, 0 )
             INSERT INTO bomlink VALUES (43, 'GS1',   'CH102',   3, 1.0, 0 )
             INSERT INTO bomlink VALUES (44, 'GS1',   'PE102',   4, 2.0, 0 )

    INSERT INTO bomlink VALUES (5, 'BI1',   'BK1',     5, NULL, 0 )
             INSERT INTO bomlink VALUES (51, 'BK1',   'BC101',   1, 2.0, 0 )
             INSERT INTO bomlink VALUES (52, 'BK1',   'BP101',   2, 2.0, 0 )
             INSERT INTO bomlink VALUES (53, 'BK1',   'BD102',   3, 2.0, 0 )

    INSERT INTO bomlink VALUES (1000, 'root',  'BI2',   2, NULL , 1)

    INSERT INTO bomlink VALUES (1001, 'BI2',   'FR2',   1, NULL , 1)
             INSERT INTO bomlink VALUES (1011, 'FR2',  'FB103',   1, 1.0, 0 )
             INSERT INTO bomlink VALUES (1012, 'FR2',  'HA103',   2, 1.0, 0 )
             INSERT INTO bomlink VALUES (1013, 'FR2',  'SA102',   3, 1.0, 0 )

END FUNCTION

FUNCTION __mbox_ok(title,message,icon)
  DEFINE title, message, icon STRING
  MENU title ATTRIBUTES(STYLE='dialog',IMAGE=icon,COMMENT=message)
     COMMAND "OK"
  END MENU
END FUNCTION

FUNCTION __mbox_yn(title,message,icon)
  DEFINE title, message, icon STRING
  DEFINE r SMALLINT
  MENU title ATTRIBUTES(STYLE='dialog',IMAGE=icon,COMMENT=message)
     COMMAND "Yes" LET r=TRUE
     COMMAND "No"  LET r=FALSE
  END MENU
  RETURN r
END FUNCTION

FUNCTION __mbox_ync(title,message,icon)
  DEFINE title, message, icon STRING
  DEFINE r CHAR
  MENU title ATTRIBUTES(STYLE='dialog',IMAGE=icon,COMMENT=message)
     COMMAND "Yes"     LET r="y"
     COMMAND "No"      LET r="n"
     COMMAND "Cancel"  LET r="c"
  END MENU             
  RETURN r
END FUNCTION


