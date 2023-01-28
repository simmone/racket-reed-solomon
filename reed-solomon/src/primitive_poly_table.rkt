#lang racket

(provide (contract-out
          [*primitive_poly_table* (hash/c natural? string?)]
          ))

(define *primitive_poly_table*
  #hash(
        (7 . "x2+x1+1")
        (11 . "x3+x1+1")
        (19 . "x4+x1+1")
        (37 . "x5+x2+1")
        (55 . "x5+x4+x2+x1+1")
        (61 . "x5+x4+x3+x2+1")
        (67 . "x6+x1+1")
        (103 . "x6+x5+x2+x1+1")
        (109 . "x6+x5+x3+x2+1")
        (131 . "x7+x1+1")
        (137 . "x7+x3+1")
        (143 . "x7+x3+x2+x1+1")
        (157 . "x7+x4+x3+x2+1")
        (191 . "x7+x5+x4+x3+x2+x1+1")
        (203 . "x7+x6+x3+x1+1")
        (213 . "x7+x6+x4+x2+1")
        (229 . "x7+x6+x5+x2+1")
        (247 . "x7+x6+x5+x4+x2+x1+1")
        (285 . "x8+x4+x3+x2+1")
        (299 . "x8+x5+x3+x1+1")
        (351 . "x8+x6+x4+x3+x2+x1+1")
        (355 . "x8+x6+x5+x1+1")
        (357 . "x8+x6+x5+x2+1")
        (361 . "x8+x6+x5+x3+1")
        (451 . "x8+x7+x6+x1+1")
        (487 . "x8+x7+x6+x5+x2+x1+1")
        (529 . "x9+x4+1")
        (557 . "x9+x5+x3+x2+1")
        (601 . "x9+x6+x4+x3+1")
        (623 . "x9+x6+x5+x3+x2+x1+1")
        (631 . "x9+x6+x5+x4+x2+x1+1")
        (731 . "x9+x7+x6+x4+x3+x1+1")
        (787 . "x9+x8+x4+x1+1")
        (817 . "x9+x8+x5+x4+1")
        (865 . "x9+x8+x6+x5+1")
        (875 . "x9+x8+x6+x5+x3+x1+1")
        (901 . "x9+x8+x7+x2+1")
        (911 . "x9+x8+x7+x3+x2+x1+1")
        (995 . "x9+x8+x7+x6+x5+x1+1")
        (1001 . "x9+x8+x7+x6+x5+x3+1")
        (1033 . "x10+x3+1")
        (1051 . "x10+x4+x3+x1+1")
        (1135 . "x10+x6+x5+x3+x2+x1+1")
        (1293 . "x10+x8+x3+x2+1")
        (1305 . "x10+x8+x4+x3+1")
        (1315 . "x10+x8+x5+x1+1")
        (1329 . "x10+x8+x5+x4+1")
        (1509 . "x10+x8+x7+x6+x5+x2+1")
        (1531 . "x10+x8+x7+x6+x5+x4+x3+x1+1")
        (1555 . "x10+x9+x4+x1+1")
        (1663 . "x10+x9+x6+x5+x4+x3+x2+x1+1")
        (1869 . "x10+x9+x8+x6+x3+x2+1")
        (1891 . "x10+x9+x8+x6+x5+x1+1")
        (2041 . "x10+x9+x8+x7+x6+x5+x4+x3+1")
        (2053 . "x11+x2+1")
        (2091 . "x11+x5+x3+x1+1")
        (2093 . "x11+x5+x3+x2+1")
        (2147 . "x11+x6+x5+x1+1")
        (2189 . "x11+x7+x3+x2+1")
        (2341 . "x11+x8+x5+x2+1")
        (2419 . "x11+x8+x6+x5+x4+x1+1")
        (2431 . "x11+x8+x6+x5+x4+x3+x2+x1+1")
        (2579 . "x11+x9+x4+x1+1")
        (2963 . "x11+x9+x8+x7+x4+x1+1")
        (3085 . "x11+x10+x3+x2+1")
        (3227 . "x11+x10+x7+x4+x3+x1+1")
        (3515 . "x11+x10+x8+x7+x5+x4+x3+x1+1")
        (3851 . "x11+x10+x9+x8+x3+x1+1")
        (4179 . "x12+x6+x4+x1+1")
        (4621 . "x12+x9+x3+x2+1")
        (4879 . "x12+x9+x8+x3+x2+x1+1")
        (5957 . "x12+x10+x9+x8+x6+x2+1")
        (6005 . "x12+x10+x9+x8+x6+x5+x4+x2+1")
        (6231 . "x12+x11+x6+x4+x2+x1+1")
        (6699 . "x12+x11+x9+x5+x3+x1+1")
        (6865 . "x12+x11+x9+x7+x6+x4+1")
        (6881 . "x12+x11+x9+x7+x6+x5+1")
        (7057 . "x12+x11+x9+x8+x7+x4+1")
        (7079 . "x12+x11+x9+x8+x7+x5+x2+x1+1")
        (7207 . "x12+x11+x10+x5+x2+x1+1")
        (7515 . "x12+x11+x10+x8+x6+x4+x3+x1+1")
        (8123 . "x12+x11+x10+x9+x8+x7+x5+x4+x3+x1+1")
        (8219 . "x13+x4+x3+x1+1")
        (8895 . "x13+x9+x7+x5+x4+x3+x2+x1+1")
        (9123 . "x13+x9+x8+x7+x5+x1+1")
        (9905 . "x13+x10+x9+x7+x5+x4+1")
        (10063 . "x13+x10+x9+x8+x6+x3+x2+x1+1")
        (10643 . "x13+x11+x8+x7+x4+x1+1")
        (12287 . "x13+x11+x10+x9+x8+x7+x6+x5+x4+x3+x2+x1+1")
        (12409 . "x13+x12+x6+x5+x4+x3+1")
        (12769 . "x13+x12+x8+x7+x6+x5+1")
        (13077 . "x13+x12+x9+x8+x4+x2+1")
        (13661 . "x13+x12+x10+x8+x6+x4+x3+x2+1")
        (14375 . "x13+x12+x11+x5+x2+x1+1")
        (14803 . "x13+x12+x11+x8+x7+x6+x4+x1+1")
        (14889 . "x13+x12+x11+x9+x5+x3+1")
        (16707 . "x14+x8+x6+x1+1")
        (17475 . "x14+x10+x6+x1+1")
        (18139 . "x14+x10+x9+x7+x6+x4+x3+x1+1")
        (18499 . "x14+x11+x6+x1+1")
        (19045 . "x14+x11+x9+x6+x5+x2+1")
        (21489 . "x14+x12+x9+x8+x7+x6+x5+x4+1")
        (23531 . "x14+x12+x11+x9+x8+x7+x6+x5+x3+x1+1")
        (24217 . "x14+x12+x11+x10+x9+x7+x4+x3+1")
        (24683 . "x14+x13+x6+x5+x3+x1+1")
        (26047 . "x14+x13+x10+x8+x7+x5+x4+x3+x2+x1+1")
        (26743 . "x14+x13+x11+x6+x5+x4+x2+x1+1")
        (26927 . "x14+x13+x11+x8+x5+x3+x2+x1+1")
        (31939 . "x14+x13+x12+x11+x10+x7+x6+x1+1")
        (32353 . "x14+x13+x12+x11+x10+x9+x6+x5+1")
        (32771 . "x15+x1+1")
        (32785 . "x15+x4+1")
        (32897 . "x15+x7+1")
        (32975 . "x15+x7+x6+x3+x2+x1+1")
        (33827 . "x15+x10+x5+x1+1")
        (33841 . "x15+x10+x5+x4+1")
        (33847 . "x15+x10+x5+x4+x2+x1+1")
        (34473 . "x15+x10+x9+x7+x5+x3+1")
        (34601 . "x15+x10+x9+x8+x5+x3+1")
        (35015 . "x15+x11+x7+x6+x2+x1+1")
        (36875 . "x15+x12+x3+x1+1")
        (36925 . "x15+x12+x5+x4+x3+x2+1")
        (39381 . "x15+x12+x11+x8+x7+x6+x4+x2+1")
        (65533 . "x15+x14+x13+x12+x11+x10+x9+x8+x7+x6+x5+x4+x3+x2+1")
        (66525 . "x16+x9+x8+x7+x6+x4+x3+x2+1")
        (69643 . "x16+x12+x3+x1+1")
        (69765 . "x16+x12+x7+x2+1")
        (79555 . "x16+x13+x12+x10+x9+x7+x6+x1+1")
        (80075 . "x16+x13+x12+x11+x7+x6+x3+x1+1")
        (80967 . "x16+x13+x12+x11+x10+x6+x2+x1+1")
        (83211 . "x16+x14+x10+x8+x3+x1+1")
        (94317 . "x16+x14+x13+x12+x6+x5+x3+x2+1")
        (95361 . "x16+x14+x13+x12+x10+x7+1")
        (99439 . "x16+x15+x10+x6+x5+x3+x2+x1+1")
        (101303 . "x16+x15+x11+x9+x8+x7+x5+x4+x2+x1+1")
        (101615 . "x16+x15+x11+x10+x7+x6+x5+x3+x2+x1+1")
        (101959 . "x16+x15+x11+x10+x9+x6+x2+x1+1")
        (102231 . "x16+x15+x11+x10+x9+x8+x6+x4+x2+x1+1")
        (131081 . "x17+x3+1")
        (131087 . "x17+x3+x2+x1+1")
        (131105 . "x17+x5+1")
        (131137 . "x17+x6+1")
        (131353 . "x17+x8+x4+x3+1")
        (131545 . "x17+x8+x7+x6+x4+x3+1")
        (132973 . "x17+x10+x9+x8+x6+x5+x3+x2+1")
        (135247 . "x17+x12+x6+x3+x2+x1+1")
        (135743 . "x17+x12+x9+x5+x4+x3+x2+x1+1")
        (135901 . "x17+x12+x9+x7+x6+x4+x3+x2+1")
        (149679 . "x17+x14+x11+x7+x5+x3+x2+x1+1")
        (174761 . "x17+x15+x13+x11+x9+x7+x5+x3+1")
        (174807 . "x17+x15+x13+x11+x9+x7+x6+x4+x2+x1+1")
        (196619 . "x17+x16+x3+x1+1")
        (262207 . "x18+x5+x4+x3+x2+x1+1")
        (262273 . "x18+x7+1")
        (262311 . "x18+x7+x5+x2+x1+1")
        (262407 . "x18+x8+x2+x1+1")
        (262897 . "x18+x9+x7+x6+x5+x4+1")
        (263031 . "x18+x9+x8+x6+x5+x4+x2+x1+1")
        (263127 . "x18+x9+x8+x7+x6+x4+x2+x1+1")
        (263329 . "x18+x10+x7+x5+1")
        (263457 . "x18+x10+x8+x5+1")
        (263679 . "x18+x10+x8+x7+x6+x5+x4+x3+x2+x1+1")
        (263689 . "x18+x10+x9+x3+1")
        (270417 . "x18+x13+x6+x4+1")
        (294949 . "x18+x15+x5+x2+1")
        (295429 . "x18+x15+x9+x2+1")
        (524327 . "x19+x5+x2+x1+1")
        (524351 . "x19+x5+x4+x3+x2+x1+1")
        (524359 . "x19+x6+x2+x1+1")
        (524399 . "x19+x6+x5+x3+x2+x1+1")
        (524413 . "x19+x6+x5+x4+x3+x2+1")
        (524463 . "x19+x7+x5+x3+x2+x1+1")
        (524705 . "x19+x8+x7+x5+1")
        (524735 . "x19+x8+x7+x5+x4+x3+x2+x1+1")
        (524767 . "x19+x8+x7+x6+x4+x3+x2+x1+1")
        (525089 . "x19+x9+x8+x5+1")
        (525167 . "x19+x9+x8+x6+x5+x3+x2+x1+1")
        (525215 . "x19+x9+x8+x7+x4+x3+x2+x1+1")
        (527357 . "x19+x11+x9+x8+x7+x6+x5+x4+x3+x2+1")
        (527807 . "x19+x11+x10+x8+x7+x5+x4+x3+x2+x1+1")
        (599187 . "x19+x16+x13+x10+x7+x4+x1+1")
        (1048585 . "x20+x3+1")
        (1049129 . "x20+x9+x5+x3+1")
        (1050957 . "x20+x11+x8+x6+x3+x2+1")
        (1066865 . "x20+x14+x10+x9+x8+x6+x5+x4+1")
        (1197213 . "x20+x17+x14+x10+x7+x4+x3+x2+1")
        (1572889 . "x20+x19+x4+x3+1")
        (2097157 . "x21+x2+1")
        (2097565 . "x21+x8+x7+x4+x3+x2+1")
        (2098269 . "x21+x10+x6+x4+x3+x2+1")
        (2105381 . "x21+x13+x5+x2+1")
        (2113669 . "x21+x14+x7+x2+1")
        (2113741 . "x21+x14+x7+x6+x3+x2+1")
        (2117853 . "x21+x14+x12+x7+x6+x4+x3+x2+1")
        (2131517 . "x21+x15+x10+x9+x5+x4+x3+x2+1")
        (3932221 . "x21+x20+x19+x18+x5+x4+x3+x2+1")
        (4194307 . "x22+x1+1")
        (4194851 . "x22+x9+x5+x1+1")
        (4223119 . "x22+x14+x13+x12+x7+x3+x2+x1+1")
        (4326023 . "x22+x17+x9+x7+x2+x1+1")
        (4338055 . "x22+x17+x13+x12+x8+x7+x2+x1+1")
        (5570647 . "x22+x20+x18+x16+x6+x4+x2+x1+1")
        (8388641 . "x23+x5+1")
        (8388659 . "x23+x5+x4+x1+1")
        (8391905 . "x23+x11+x10+x7+x6+x5+1")
        (8392753 . "x23+x12+x5+x4+1")
        (8423097 . "x23+x15+x10+x9+x7+x5+x4+x3+1")
        (8462441 . "x23+x16+x13+x6+x5+x3+1")
        (8521761 . "x23+x17+x11+x5+1")
        (8522547 . "x23+x17+x11+x9+x8+x5+x4+x1+1")
        (8726821 . "x23+x18+x16+x13+x11+x8+x5+x2+1")
        (10485921 . "x23+x21+x7+x5+1")
        (16777351 . "x24+x7+x2+x1+1")
        (19916339 . "x24+x21+x19+x18+x17+x16+x15+x14+x13+x10+x9+x5+x4+x1+1")
        (22367153 . "x24+x22+x20+x18+x16+x14+x11+x9+x8+x7+x5+x4+1")
        (33554441 . "x25+x3+1")
        (33554447 . "x25+x3+x2+x1+1")
        (33557341 . "x25+x11+x9+x8+x6+x4+x3+x2+1")
        (33558553 . "x25+x12+x4+x3+1")
        (33561049 . "x25+x12+x11+x8+x7+x6+x4+x3+1")
        (33686543 . "x25+x17+x10+x3+x2+x1+1")
        (33822841 . "x25+x18+x12+x11+x6+x5+x4+x3+1")
        (34603049 . "x25+x20+x5+x3+1")
        (34670639 . "x25+x20+x16+x11+x5+x3+x2+x1+1")
        (44565161 . "x25+x23+x21+x19+x9+x7+x5+x3+1")
        (67108935 . "x26+x6+x2+x1+1")
        (67759085 . "x26+x19+x16+x15+x14+x13+x11+x9+x8+x7+x6+x5+x3+x2+1")
        (69581689 . "x26+x21+x18+x16+x15+x13+x12+x11+x9+x8+x6+x5+x4+x3+1")
        (72952759 . "x26+x22+x20+x19+x16+x13+x11+x9+x8+x7+x5+x4+x2+x1+1")
        (73473339 . "x26+x22+x21+x16+x12+x11+x10+x8+x5+x4+x3+x1+1")
        (82636645 . "x26+x23+x22+x21+x19+x18+x15+x14+x13+x11+x10+x9+x8+x6+x5+x2+1")
        (86206675 . "x26+x24+x21+x17+x16+x14+x13+x11+x7+x6+x4+x1+1")
        (134217767 . "x27+x5+x2+x1+1")
        (134483513 . "x27+x18+x11+x10+x9+x5+x4+x3+1")
        (138422393 . "x27+x22+x13+x11+x6+x5+x4+x3+1")
        (138600515 . "x27+x22+x17+x15+x14+x13+x6+x1+1")
        (141996193 . "x27+x22+x21+x20+x18+x17+x15+x13+x12+x7+x5+1")
        (151589263 . "x27+x24+x19+x16+x12+x8+x7+x3+x2+x1+1")
        (153692793 . "x27+x24+x21+x19+x16+x13+x11+x9+x6+x5+x4+x3+1")
        (178269167 . "x27+x25+x23+x21+x13+x11+x9+x8+x7+x6+x5+x3+x2+x1+1")
        (180176281 . "x27+x25+x23+x21+x20+x19+x18+x16+x14+x10+x8+x7+x4+x3+1")
        (268435465 . "x28+x3+1")
        (268446249 . "x28+x13+x11+x9+x5+x3+1")
        (268894777 . "x28+x18+x17+x16+x9+x5+x4+x3+1")
        (269124685 . "x28+x19+x17+x15+x10+x6+x3+x2+1")
        (272632857 . "x28+x22+x11+x10+x4+x3+1")
        (286331161 . "x28+x24+x20+x16+x12+x8+x4+x3+1")
        (536870917 . "x29+x2+1")
        (536875141 . "x29+x12+x7+x2+1")
        (537149517 . "x29+x18+x14+x6+x3+x2+1")
        (537460813 . "x29+x19+x16+x6+x3+x2+1")
        (537921541 . "x29+x20+x11+x2+1")
        (537987357 . "x29+x20+x16+x11+x8+x4+x3+x2+1")
        (538968101 . "x29+x21+x5+x2+1")
        (545261117 . "x29+x23+x10+x9+x5+x4+x3+x2+1")
        (553672989 . "x29+x24+x14+x13+x8+x4+x3+x2+1")
        (603979813 . "x29+x26+x5+x2+1")
        (1082130439 . "x30+x23+x2+x1+1")
        (1091659911 . "x30+x24+x20+x16+x14+x13+x11+x7+x2+x1+1")
        (1093972699 . "x30+x24+x21+x20+x18+x15+x13+x12+x9+x7+x6+x4+x3+x1+1")
        (1133332827 . "x30+x25+x24+x23+x19+x18+x16+x14+x11+x8+x6+x4+x3+x1+1")
        (1271469507 . "x30+x27+x25+x24+x23+x22+x19+x16+x12+x10+x8+x7+x6+x1+1")
        (2147483657 . "x31+x3+1")
        (2147483663 . "x31+x3+x2+x1+1")
        (2147492105 . "x31+x13+x8+x3+1")
        (2147549469 . "x31+x16+x8+x4+x3+x2+1")
        (2148565049 . "x31+x20+x15+x5+x4+x3+1")
        (2148794537 . "x31+x20+x18+x7+x5+x3+1")
        (2149584911 . "x31+x21+x12+x3+x2+x1+1")
        (2160115865 . "x31+x23+x22+x15+x14+x7+x4+x3+1")
        (2181578895 . "x31+x25+x19+x14+x7+x3+x2+x1+1")
        (2290649225 . "x31+x27+x23+x19+x15+x11+x7+x3+1")
        (2290650863 . "x31+x27+x23+x19+x15+x11+x10+x9+x7+x6+x5+x3+x2+x1+1")
        (4299161607 . "x32+x22+x2+x1+1")
        (4302746963 . "x32+x22+x21+x20+x18+x17+x15+x13+x12+x10+x8+x6+x4+x1+1")
        (4303570409 . "x32+x23+x17+x16+x14+x10+x8+x7+x6+x5+x3+1")
        (4374732215 . "x32+x26+x23+x22+x16+x12+x11+x10+x8+x7+x5+x4+x2+x1+1")
        (4559351687 . "x32+x27+x26+x25+x24+x23+x22+x17+x13+x11+x10+x9+x8+x7+x2+x1+1")
        (4564274787 . "x32+x28+x19+x18+x16+x14+x11+x10+x9+x6+x5+x1+1")
        ))