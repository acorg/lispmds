(in-package user)

(setq robin-codons
  '(121
    124
    133
    135
    138
    142
    145
    156
    158
    186
    190
    193
    194
    197
    201
    226
    262
    275))

#|

From http://www.sciencemag.org/feature/data/1044188.shl

Supplemental Table 1. Codons in the HA1 hemagglutinin gene used (+, A, B) in prediction tests. P, codons under positive selection; R, codons in the sialic acid receptor binding site; A, codons in or near antibody combining site A or B; F, codons with rapid rates of amino acid replacement; F-P, codons with rapid rates of amino acid replacement and not under positive selection.


(derek note, there are some +'s in the F-P col, with -'s in F col.  perhaps this is an error,
       but there are 6 or so, so likely not.  maybe the F-P col is those F's which are under
       negatative selection?)


Codon	P	R	A	F	F-P
50	- 	- 	- 	- 	+         
80	- 	- 	- 	- 	+
98	- 	+	- 	- 	-
121	+	- 	- 	+	-
122	- 	- 	A	- 	-
124	+	- 	A	- 	-
126	- 	- 	A	- 	-
128	- 	- 	B	- 	+
129	- 	- 	B	- 	-
130	- 	- 	A	- 	-
131	- 	- 	A	- 	-
132	- 	- 	A	- 	-
133	+	- 	A	+	-
134	- 	+	- 	- 	-
135	+	+	A	+	-
136	- 	+	- 	- 	-
137	- 	+	A	+	+
138	+	+	A	+	-
140	- 	- 	A	- 	-
142	+	- 	A	- 	-
143	- 	- 	A	- 	-
144	- 	- 	A	- 	-
145	+	- 	A	+	-
146	- 	- 	A	- 	-
150	- 	- 	A	- 	-
152	- 	- 	A	- 	-
153	- 	+	- 	- 	-
155	- 	+	B	- 	-
156	+	- 	B	+	-
157	- 	- 	B	- 	-
158	+	- 	B	- 	-
159	- 	- 	B	+	+
160	- 	- 	B	- 	-
163	- 	- 	B	- 	+
164	- 	- 	B	- 	-
165	- 	- 	B	- 	-
168	- 	- 	A	- 	-
172	- 	- 	- 	- 	+
182	- 	- 	- 	- 	+
183	- 	+	- 	- 	-
186	+	- 	B	+	-
187	- 	- 	B	- 	-
188	- 	- 	B	- 	-
189	- 	- 	B	- 	-
190	+	+	B	+	-
192	- 	- 	B	- 	-
193	+	- 	B	+	-
194	+	+	B	+	-
196	- 	- 	B	+	+
197	+	- 	B	- 	-
198	- 	- 	B	- 	+
201	+	- 	- 	- 	-
214	- 	- 	- 	- 	+
219	- 	- 	- 	+	+
224	- 	+	- 	- 	-
225	- 	+	- 	- 	-
226	+	+	- 	+	-
227	- 	+	- 	- 	-
228	- 	+	- 	- 	-
229	- 	- 	- 	+	+
246	- 	- 	- 	+	+
248	- 	- 	- 	+	+
262	+	- 	- 	- 	-
275	+	- 	- 	+	-
276	- 	- 	- 	+	+
310	- 	- 	- 	- 	+
312	- 	- 	- 	- 	+
N	8	16	41	20	18



Site C (n = 27): 44-48, 50, 51, 53, 54, 273, 275, 276, 278-280, 294, 297, 299, 300, 304, 305, 307-312

Site D (n = 41): 96, 102, 103, 117, 121, 167, 170- 177, 179, 182, 201, 203, 207-209, 212-219, 226-230,
                        238, 240, 242, 244, 246-248

Site E (n = 22): 57, 59, 62, 63, 67, 75, 78, 80-83, 86-88, 91, 92, 94, 109, 260-262, 265



|#


(progn
  (setq h3-site-a '(122
		    124
		    126
		    130
		    131
		    132
		    133
		    135
		    137
		    138
		    140
		    142
		    143
		    144
		    145
		    146
		    150
		    152
		    168))

  (setq h3-site-b '(128
		    129
		    155
		    156
		    157
		    158
		    159
		    160
		    163
		    164
		    165
		    186
		    187
		    188
		    189
		    190
		    192
		    193
		    194
		    196
		    197
		    198))

  (setq h3-site-c '(44 45 46 47 48 50 51 53 54 273 275 276 278 279 280 294 297 299 300 304 305 307 308 309 310 311 312))

  (setq h3-site-d '(96 102 103 117 121 167 170 171 172 173 174 175 176 177 179 182 201 203 207 208 209 212 213 214 215
		       216 217 218 219 226 227 228 229 230 238 240 242 244 246 247 248))

  (setq h3-site-e '(57 59 62 63 67 75 78 80 81 82 83 86 87 88 91 92 94 109 260 261 262 265))

  (setq h3-sites (list h3-site-a
		       h3-site-b
		       h3-site-c
		       h3-site-d
		       h3-site-e))
  )
