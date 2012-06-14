type punkt=P of float * float;;
let maxx = 1.0
let maxy = 1.0
let minx = -1.0
let miny = -1.0
let punkte=[P(0.0,0.0);P(0.1,0.1);P(0.3,0.3);P(0.2,0.2);P(0.4,0.4)]
let dist(P(x1,y1),P(x2,y2))=sqrt((x2-.x1)**2.0+.(y2-.y1)**2.0);;
let sort(pl)= List.sort (fun x y -> int_of_float(10. *. dist(x,y))) pl;;
