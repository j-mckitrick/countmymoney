function V() {}

V.init = function()
{
	var paper = Raphael(10, 50, 800, 800);
//	var paper = Raphael('container');
//	paper.g.piechart(320, 240, 100, [55, 20, 13, 32, 5, 1, 2]);
	paper.g.linechart(100, 100, 400, 400,
					  [0, 1, 2, 3, 4, 5, 6],
					  [55, 20, 13, 32, 34, 64, 22],
					  { axis: "0 0 1 1" });

	var ax = [0, 1, 2, 3, 4];
		+ax[0];
		+ax[1];
}
