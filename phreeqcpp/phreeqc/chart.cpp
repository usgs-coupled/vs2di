/*
 Creates chart with curves, using J. Champion's zedgraph:
 http://zedgraph.org/
 Define CHART in global.h. Must be compiled with /clr. Needs Form1.h.
 zedgraph.dll must be in the exe dir.
 Parameter reading follows Vincent Post's USER_GRAPH,
 #ifdef PHREEQ98 was changed in some places to #if defined PHREEQ98 || defined CHART
 not used: t->Join. the user must press Alt-F4.
 todo: include the dll's;
*/
#if !defined(PHREEQC_CLASS)
#define EXTERNAL extern
#include "global.h"
#include <malloc.h>
#include <string>
#include <fstream>
#else
#include "Phreeqc.h"

#endif
#include "phqalloc.h"
#include "output.h"
#include "phrqproto.h"
#include "input.h"

//#include <process.h> /* _beginthread(proc, stack, (void *) arglist) */

#ifdef CHART
			
/* #define CHECK_NA (float) 1e-3; */
#if !defined(PHREEQC_CLASS)
	#define NA (float) -9.9999	            /* NA = not available */
	int update_time_chart = 150;			/* milliseconds, maybe read */
	int PanelHeight = 510;//510;
	int PanelWidth = 640;//640;

	char *axis_titles[3] =  {"X-axis", "Y-axis", "Y2-axis"};
	float axis_scale_x[5] = {NA, NA, NA, NA, NA}; /* min, max, major tic, minor tic, log */
	float axis_scale_y[5] = {NA, NA, NA, NA, NA};
	float axis_scale_y2[5] = {NA, NA, NA, NA, NA};
	void SetAxisScale(char *a, int j, char *token, int true_); /* fills axis_scale_x, .._y and .._y2 */
	char *chart_title = "";

	int chart_type = 0;						/* default: plot vs distance. If chart_type = 1, plot vs time */
	int graph_initial_solutions = 0;		/* false */
	int connect_simulations = 1;			/* same curve properties in new simulations */
	int rownr = -1;
	int colnr = 0;							/* row and col no defined in basic.c for GridChar and Plot_XY */
	int RowOffset = 0;						/* = 1 if new simulations should add points to the same curve */
	int ColumnOffset = 0;					/* sets column offset, from CSV plot, and from new USER_GRAPH */
	int prev_advection_step = 0;
	int prev_transport_step = 0;			/* for different curve properties in c-x plots in a simulation */
	int AddSeries = 1;						/* new curve properties in new simulation (does the same, but opposite of connect_simulation) */

	int prev_sim_no;						/* set in new simulation, used for changing curve properties */
	bool x_filled, col_dwn, y_filled[20]; 	/* in case 20 graph_x is defined after 10 graph_y, perhaps fails when curvenr > 20... */
	float x_value;							/* from graph_x */
	void GridChar(char *s, char *a);		/* compatible with PfW's graph_x etc.: translates values to Curves(x, y) */
	void PlotXY(char *x, char *y);			/* translates from plot_xy x, y, Color = Red... */
	bool all_points;						/* true if points in curves remain the same. Works with end_timer */
	bool end_timer = false;					/* in mainsubs.c, stops the update timer in form1.h */

	struct Curves_c;						/* defines the Curves */
	int ncurves = 0;						/* number of malloced curves */
	int ncurves_changed[3] = {0, 0, 0};		/* for updating the chart:
											0 or 1 (if curves have changed), previous no, new no of curves with points*/
	void MallocCurves(int nc, int ncxy);	/* mallocs nc Curves, with ncxy Curves.x, Curves.y */
	void DeleteCurves(void);				/* deletes the curves */
	void ReallocCurves(int new_nc);			/* reallocs Curves to new_nc, doubles ncurves when new_nc = 0 */
	void ReallocCurveXY(int i);				/* reallocs x,y of Curves[i] */
	void ExtractCurveInfo(char *line, int curvenr); /* gets line & symbol properties from plot_xy x, y, color = ... */
	char *SymbolList[11] =  {"Square", "Diamond", "Triangle", "Circle", "XCross", "Plus", "Star",
			"TriangleDown", "HDash", "VDash", "None"};
	/*ColorList = {"Red", "Green", "Blue", "Orange", "Magenta", "Yellow", "Black" }; // defined in Form1.h as cli */
	/* or any color from System::Drawing::Color */

	int OpenCSVFile(char file_name[MAX_LENGTH]); /* reads CSV file, may contain line + symbol properties */
	int nCSV_headers = 0;					/* no of CSV curves, also defines ColumnOffset if connect_simulations = 1 */
	void SaveCurvesToFile(char file_name[MAX_LENGTH]); /* saves all chart data to curves.u_g */
	void start_chart(bool end);				/* end = 0 starts the chart */

	int FirstCallToUSER_GRAPH = 1;
	bool new_ug = false;					/* in case USER_GRAPH is redefined */

struct Curves_c {
	float *x, *y;
	int nxy, npoints, npoints_plot, prev_npoints;
	
	char *id, *color, *symbol;
	int y_axis; 
	float line_w, symbol_size;
} *Curves;
#endif  // !PHREEQC_CLASS

void CLASS_QUALIFIER
SetChartTitle(char *s)
{
	chart_title = string_duplicate(s);
}

void CLASS_QUALIFIER
SetAxisTitles(char *s, int i)
{
	axis_titles[i] = string_duplicate(s);
}

void CLASS_QUALIFIER
SetAxisScale(char *a, int j, char *token, int true_)
{
	if (j < 4)
	{
		float f;
		if (strncmp(token, "a", 1) == 0)
			f = NA;
		else
			f = (float) atof(token);

		if (strcmp(a, "x") == 0)
			axis_scale_x[j] = f;
		else if (strcmp(a, "y") == 0)
			axis_scale_y[j] = f;
		else if (strcmp(a, "s") == 0)
			axis_scale_y2[j] = f;
	}
	else if (j == 4 && true_)
	{
		if (strcmp(a, "x") == 0)
		{
			if (axis_scale_x[0] <= 0 && !(fabs(axis_scale_x[0] - NA) < 1e-3)
					|| axis_scale_x[1] <= 0 && !(fabs(axis_scale_x[1] - NA) < 1e-3))
			{
				input_error++;
				error_msg("MIN and MAX must be > 0 for log x-scale.",
				  CONTINUE);
			}
			axis_scale_x[j] = 10.0;
		}
		else if (strcmp(a, "y") == 0)
		{
			if (axis_scale_y[0] <= 0 && !(fabs(axis_scale_y[0] - NA) < 1e-3)
				|| axis_scale_y[1] <= 0 && !(fabs(axis_scale_y[1] - NA) < 1e-3))
			{
				input_error++;
				error_msg("MIN and MAX must be > 0 for log y-scale.",
				  CONTINUE);
			}
			axis_scale_y[j] = 10.0;
		}
		else if (strcmp(a, "s") == 0)
		{
			if (axis_scale_y2[0] <= 0 && !(fabs(axis_scale_y2[0] - NA) < 1e-3)
				|| axis_scale_y2[1] <= 0 && !(fabs(axis_scale_y2[1] - NA) < 1e-3))
			{
				input_error++;
				error_msg("MIN and MAX must be > 0 for log sy-scale.",
				  CONTINUE);
			}
			axis_scale_y2[j] = 10.0;
		}
	}
}

void CLASS_QUALIFIER
MallocCurves(int nc, int ncxy)
{
	if (ncurves)
		return;
	ncurves = nc;
	Curves = new Curves_c [ncurves];
	if (Curves == NULL) malloc_error();

	for (int i = 0; i < ncurves; i++) {
		Curves[i].x = new float [ncxy];
		if (Curves[i].x == NULL) malloc_error();
		Curves[i].y = new float [ncxy];
		if (Curves[i].y == NULL) malloc_error();
		for (int i2 = 0; i2 < ncxy; i2++)
		{
			Curves[i].x[i2] = (float) NA;
			Curves[i].y[i2] = (float) NA;
		}
		Curves[i].nxy = ncxy;
		Curves[i].npoints_plot = Curves[i].npoints = Curves[i].prev_npoints = 0;
		Curves[i].id = string_duplicate("");
		Curves[i].color = Curves[i].symbol = string_hsave("");
		Curves[i].y_axis = 1;
		Curves[i].line_w = (float) 1.0;
		Curves[i].symbol_size = (float) 6.0;
	}
}

void CLASS_QUALIFIER
DeleteCurves(void)
{
	for (int i = 0; i < ncurves; i++)
	{
		delete[] Curves[i].x;
		delete[] Curves[i].y;
		PHRQ_free(Curves[i].id);
	}
	delete[] Curves;
}

void CLASS_QUALIFIER
ReallocCurves(int new_nc)
{
	int i, i2;
	int new_xy;
	if (!new_nc)
	{
		new_nc = ncurves;
		if (new_nc < 11)
			new_nc *= 2;
		else
			new_nc += 10;
	}

	Curves_c *Curves_t = new Curves_c [new_nc];
	if (Curves_t == NULL) malloc_error();

	for (i = 0; i < ncurves; i++)
	{
		Curves_t[i].x = new float [Curves[i].nxy];
		if (Curves_t[i].x == NULL) malloc_error();
		for (i2 = 0; i2 < Curves[i].nxy; i2++)
			Curves_t[i].x[i2] = Curves[i].x[i2];

		Curves_t[i].y = new float [Curves[i].nxy];
		if (Curves_t[i].y == NULL) malloc_error();
		for (i2 = 0; i2 < Curves[i].nxy; i2++)
			Curves_t[i].y[i2] = Curves[i].y[i2];

		Curves_t[i].nxy = Curves[i].nxy;
		Curves_t[i].npoints_plot = Curves[i].npoints_plot;
		Curves_t[i].npoints = Curves[i].npoints;
		Curves_t[i].prev_npoints = Curves[i].prev_npoints;

		Curves_t[i].id = string_duplicate(Curves[i].id);
		Curves_t[i].color = string_hsave(Curves[i].color);
		Curves_t[i].symbol = string_hsave(Curves[i].symbol);
		Curves_t[i].y_axis = Curves[i].y_axis;
		Curves_t[i].line_w = Curves[i].line_w;
		Curves_t[i].symbol_size = Curves[i].symbol_size;
	}
	for (; i < new_nc; i++)
	{
		new_xy = 10;
		Curves_t[i].x = new float [new_xy];
		if (Curves_t[i].x == NULL) malloc_error();
		Curves_t[i].y = new float [new_xy];
		if (Curves_t[i].y == NULL) malloc_error();

		Curves_t[i].nxy = new_xy;
		Curves_t[i].npoints_plot = Curves_t[i].npoints = Curves_t[i].prev_npoints = 0;
		for (i2 = 0; i2 < new_xy; i2++)
		{
			Curves_t[i].x[i2] = (float) NA;
			Curves_t[i].y[i2] = (float) NA;
		}
		Curves_t[i].id = string_duplicate("");
		Curves_t[i].color = Curves_t[i].symbol = string_hsave("");
		Curves_t[i].y_axis = 1;
		Curves_t[i].line_w = (float) 1.0;
		Curves_t[i].symbol_size = (float) 6.0;
	}

	DeleteCurves();
	Curves = Curves_t;
	ncurves = new_nc;
	for (i = 0; i < ncurves; i++)
	{
		Curves[i].x = Curves_t[i].x;
		Curves[i].y = Curves_t[i].y;
	}
}

void CLASS_QUALIFIER
ReallocCurveXY(int i)
{
	int i2;
	float *x, *y;
	int new_xy = Curves[i].nxy;
	if (new_xy < 31)
		new_xy *= 2;
	else
		new_xy += 30;

	x = new float [new_xy];
	if (x == NULL) malloc_error();
	for (i2 = 0; i2 < Curves[i].nxy; i2++)
		x[i2] = Curves[i].x[i2];
	for (; i2 < new_xy; i2++)
		x[i2] = (float) NA;

	y = new float [new_xy];
	if (y == NULL) malloc_error();
	for (i2 = 0; i2 < Curves[i].nxy; i2++)
		y[i2] = Curves[i].y[i2];
	for (; i2 < new_xy; i2++)
		y[i2] = (float) NA;

	delete[] Curves[i].x;
	delete[] Curves[i].y;

	Curves[i].x = x;
	Curves[i].y = y;
	Curves[i].nxy = new_xy;
}


void CLASS_QUALIFIER
ExtractCurveInfo(char *line, int curvenr)
{
	int i, l, sel;
	char *ptr, *prev_ptr, *ptr2, *ptr3 = NULL;
	char token[MAX_LENGTH];
	if (curvenr + ColumnOffset >= ncurves)
		ReallocCurves(0);
	if (user_graph_count_headings > curvenr + ColumnOffset)
	{
		PHRQ_free(Curves[curvenr + ColumnOffset].id);
		Curves[curvenr + ColumnOffset].id =
			string_duplicate(user_graph_headings[curvenr + ColumnOffset]);
	}
	ptr = line;
	/* plot_xy x, tot("Cl"), color = Red, symbol = Circle, symbol_size = 0.0, line_w = 1.0, y_axis = 2 */
	/* skip the first tokens */
	for (i = 0; i < 3; i++)
		copy_token(token, &ptr, &l);

	prev_ptr = ptr;
	while (copy_token(token, &ptr, &l) != EMPTY)
	{ /* see if token contains a curve definer... */
		str_tolower(token);
		sel = -1;
		if (!strncmp(token, "color", 5))
			sel = 0;
		else if (!strncmp(token, "symbol", 5) && strstr(token, "_si") == NULL)
			sel = 1;
		else if (!strncmp(token, "symbol_size", 8))
			sel = 2;
		else if (!strncmp(token, "line_w", 5) || !strncmp(token, "line-w", 5))
			sel = 3;
		else if (!strncmp(token, "y_axis", 5) || !strncmp(token, "y-axis", 5))
			sel = 4;
		if (sel >= 0)
		{ /* store prev_ptr for reducing BASIC line... */
			if (!ptr3)
				ptr3 = prev_ptr;
			/* obtain token after =, from 4 possibilities... */
			ptr2 = strchr(token, '=');
			if (ptr2 && strlen(ptr2) > 1)
			{ /* Color=Red */
				copy_token(token, &prev_ptr, &l);
				ptr2 = strchr(token, '=');
				copy_token(token, &(++ptr2), &l);
			}
			else
			{
				copy_token(token, &ptr, &l); /* Color= Red, or... */
				if (l == 1) /* Color = Red */
					copy_token(token, &ptr, &l);
				else if (token[0] == '=') /* Color =Red */
				{
					ptr2 = &token[1];
					copy_token(token, &ptr2, &l);
				}
			}
			ptr2 = strchr(token, ',');
			if (ptr2)
				*ptr2 = '\0';

			switch (sel)
			{
			case 0:
				Curves[curvenr + ColumnOffset].color = string_hsave(token);
				break;
			case 1:
				Curves[curvenr + ColumnOffset].symbol = string_hsave(token);
				break;
			case 2:
				Curves[curvenr + ColumnOffset].symbol_size = (float) atof(token);
				break;
			case 3:
				Curves[curvenr + ColumnOffset].line_w = (float) atof(token);
				break;
			case 4:
				Curves[curvenr + ColumnOffset].y_axis = atoi(token);
			}
		}
	prev_ptr = ptr;
	}
	if (ptr3)
	{ /* shorten line to contain BASIC statements only... */
		while (*(--ptr3) != ',') {}
		*ptr3 = '\0';
	}
}
#ifdef SKIP
int CLASS_QUALIFIER
OpenCSVFile(char file_name[MAX_LENGTH])
{
	int i, l, sel, linenr = 0;
	float x_value;
	char *ptr;
	char token[MAX_LENGTH];
	FILE *f_in;
	f_in = fopen(file_name, "r");
	if (f_in == NULL)
		return 0;
/* Get lines */
	do
	{
		i = get_line(getc_callback, f_in);
		if (i == EOF)
			return EOF;
		else if (i == EMPTY)
			continue;
		ptr = line;
		if (linenr == 0)
		{ /* get headings... */
			while (copy_token(token, &ptr, &l) != EMPTY)
				nCSV_headers++;
			/* x in 1st col is not plotted... */
			nCSV_headers--;
			/* plot csv curves first... */
			user_graph_headings =
				(char **) PHRQ_realloc(user_graph_headings,
							 (size_t) (user_graph_count_headings +
									   nCSV_headers) * sizeof(char *));
			if (user_graph_headings == NULL)
				malloc_error();
			user_graph_count_headings += nCSV_headers;
			for (int i = user_graph_count_headings - 1; i >= nCSV_headers; i--)
				user_graph_headings[i] = string_hsave(user_graph_headings[i - nCSV_headers]);
			if (nCSV_headers > ncurves)
				ReallocCurves(nCSV_headers * 2);
			ColumnOffset = nCSV_headers;

			ptr = line;
			copy_token(token, &ptr, &l);
			for (int i = 0; i < nCSV_headers; i++)
			{
				copy_token(token, &ptr, &l);
				PHRQ_free(Curves[i].id);
				Curves[i].id = user_graph_headings[i] = string_duplicate(token);
				Curves[i].line_w = 0.0;
			}

			linenr++;
			continue;
		}

		sel = copy_token(token, &ptr, &l);
		if (linenr < 6 && sel != DIGIT)
		{ /* see if token contains curve definer... */
			str_tolower(token);
			sel = -1;
			if (!strncmp(token, "color", 1))
				sel = 0;
			else if (!strncmp(token, "line_w", 1))
				sel = 1;
			else if (!strncmp(token, "symbol", 1) && strstr(token, "si") == NULL)
				sel = 2;
			else if (!strncmp(token, "symbol_s", 8))
				sel = 3;
			else if (!strncmp(token, "y_axis", 1))
				sel = 4;

			if (sel >= 0)
			{ /* read curve properties... */
				i = 0;
				while (strlen(ptr) > 0 && i < nCSV_headers)
				{
					if (strstr(ptr, "\t\t") == ptr)
					{ /* 2 tabs, undefined... */
						ptr++;
						i++;
					}
					else if (copy_token(token, &ptr, &l) != EMPTY)
					{
						switch (sel)
						{
						case 0:
							Curves[i].color = string_hsave(token);
							break;
						case 1:
							Curves[i].line_w = (float) atof(token);
							break;
						case 2:
							Curves[i].symbol = string_hsave(token);
							break;
						case 3:
							Curves[i].symbol_size = (float) atof(token);
							break;
						case 4:
							Curves[i].y_axis = (int) atoi(token);
						}
						i++;
					}
				}
				linenr++;
				continue;
			}
		}
		else
		{ /* read point values... */
			if (linenr < 6)
				linenr = 6;
			if (sel == DIGIT)
			{
				x_value = (float) atof(token);
			}
			else
			{
				linenr++;
				continue;
			}
			i = 0;
			while (strlen(ptr) > 0 && i < nCSV_headers)
			{
				if (strstr(ptr, "\t\t") == ptr)
				{ /* 2 tabs, undefined... */
					ptr++;
					i++;
				}
				else if (copy_token(token, &ptr, &l) == DIGIT)
				{
					Curves[i].x[Curves[i].npoints] = x_value;
					Curves[i].y[Curves[i].npoints] = (float) atof(token);
					Curves[i].npoints++;
					if (Curves[i].npoints >= Curves[i].nxy)
						ReallocCurveXY(i);
					i++;
				}
			}
			linenr++;
		}
	}
	while (OK);

	return OK;
}
#endif
int CLASS_QUALIFIER
OpenCSVFile(char file_name[MAX_LENGTH])
{
	int i, l, sel, linenr = 0;
	float x_value;
	char *ptr;
	char token[MAX_LENGTH];

	std::ifstream f_csv(file_name, std::ifstream::in);
	if (!f_csv.is_open())
	{
		sprintf(error_string, "Could not open csv file for USER_GRAPH, %s\n", file_name);
		warning_msg(error_string);
		return 0;
	}
	std::string stdline;


/* Get lines */
	while (std::getline(f_csv, stdline))
	{
		if (stdline.size() == 0) continue;
		strcpy(line, stdline.c_str());
		ptr = line;
		if (linenr == 0)
		{ /* get headings... */
			while (copy_token(token, &ptr, &l) != EMPTY)
				nCSV_headers++;
			/* x in 1st col is not plotted... */
			nCSV_headers--;
			/* plot csv curves first... */
			user_graph_headings =
				(char **) PHRQ_realloc(user_graph_headings,
							 (size_t) (user_graph_count_headings +
									   nCSV_headers) * sizeof(char *));
			if (user_graph_headings == NULL)
				malloc_error();
			user_graph_count_headings += nCSV_headers;
			for (int i = user_graph_count_headings - 1; i >= nCSV_headers; i--)
				user_graph_headings[i] = string_hsave(user_graph_headings[i - nCSV_headers]);
			if (nCSV_headers > ncurves)
				ReallocCurves(nCSV_headers * 2);
			ColumnOffset = nCSV_headers;

			ptr = line;
			copy_token(token, &ptr, &l);
			for (int i = 0; i < nCSV_headers; i++)
			{
				copy_token(token, &ptr, &l);
				PHRQ_free(Curves[i].id);
				Curves[i].id = user_graph_headings[i] = string_duplicate(token);
				Curves[i].line_w = 0.0;
			}

			linenr++;
			continue;
		}

		sel = copy_token(token, &ptr, &l);
		if (linenr < 6 && sel != DIGIT)
		{ /* see if token contains curve definer... */
			str_tolower(token);
			sel = -1;
			if (!strncmp(token, "color", 1))
				sel = 0;
			else if (!strncmp(token, "line_w", 1))
				sel = 1;
			else if (!strncmp(token, "symbol", 1) && strstr(token, "si") == NULL)
				sel = 2;
			else if (!strncmp(token, "symbol_s", 8))
				sel = 3;
			else if (!strncmp(token, "y_axis", 1))
				sel = 4;

			if (sel >= 0)
			{ /* read curve properties... */
				i = 0;
				while (strlen(ptr) > 0 && i < nCSV_headers)
				{
					if (strstr(ptr, "\t\t") == ptr)
					{ /* 2 tabs, undefined... */
						ptr++;
						i++;
					}
					else if (copy_token(token, &ptr, &l) != EMPTY)
					{
						switch (sel)
						{
						case 0:
							Curves[i].color = string_hsave(token);
							break;
						case 1:
							Curves[i].line_w = (float) atof(token);
							break;
						case 2:
							Curves[i].symbol = string_hsave(token);
							break;
						case 3:
							Curves[i].symbol_size = (float) atof(token);
							break;
						case 4:
							Curves[i].y_axis = (int) atoi(token);
						}
						i++;
					}
				}
				linenr++;
				continue;
			}
		}
		else
		{ /* read point values... */
			if (linenr < 6)
				linenr = 6;
			if (sel == DIGIT)
			{
				x_value = (float) atof(token);
			}
			else
			{
				linenr++;
				continue;
			}
			i = 0;
			while (strlen(ptr) > 0 && i < nCSV_headers)
			{
				if (strstr(ptr, "\t\t") == ptr)
				{ /* 2 tabs, undefined... */
					ptr++;
					i++;
				}
				else if (copy_token(token, &ptr, &l) == DIGIT)
				{
					Curves[i].x[Curves[i].npoints] = x_value;
					Curves[i].y[Curves[i].npoints] = (float) atof(token);
					Curves[i].npoints++;
					if (Curves[i].npoints >= Curves[i].nxy)
						ReallocCurveXY(i);
					i++;
				}
			}
			linenr++;
		}
	}
	return EOF;
}

void CLASS_QUALIFIER
SaveCurvesToFile(char file_name[MAX_LENGTH])
{
	int i, i2;
	FILE *f_out;
  f_out = fopen(file_name, "w");
	if (f_out == NULL)
		return;
	int max_points = 0;
	for (i = 0; i < ncurves; i++)
	{
		if (!Curves[i].npoints)
			continue;
		if (strlen(Curves[i].id))
			fprintf(f_out, " x \t%s\t", Curves[i].id);
		else
			fprintf(f_out, " x \t y \t");
		if (Curves[i].npoints > max_points)
			max_points = Curves[i].npoints;
	}
	fprintf(f_out, "\n");
	i2 = 0;
	while (i2 < max_points)
	{
		for (i = 0; i < ncurves; i++)
		{
			if (!Curves[i].npoints)
				continue;
			if (i2 < Curves[i].npoints)
			{
				fprintf(f_out, "%12.4e\t", Curves[i].x[i2]);
				fprintf(f_out, "%12.4e\t", Curves[i].y[i2]);
			}
			else if (i2 < max_points)
			{
				fprintf(f_out, "\t\t");
			}
		}
		fprintf(f_out, "\n");
		i2++;
	}

	fclose(f_out);
	return;
}

#include "Form1.h"

using namespace zdg_ui2;

void  CLASS_QUALIFIER
GridChar(char *s, char *a)
{
	/* Attribute values from graph_x, graph_y, graph_sy to Curves(*x, *y, id, y_axis, etc.
	   Take care when x is not in column 0... */

	bool x, new_sim = false;
	int i;

	if (colnr == 0)
	{
		x_value = (float) NA;
		x_filled = false;
		col_dwn = false;
		for (i = 0; i < 20; i++)
			y_filled[i] = false;
	}

	if (strcmp(a, "x") == 0)
		x = true;
	else
		x = false;

	int curvenr = colnr + ColumnOffset;

	/* After the column with x, the curvenr's must be reduced by 1.
			It's a bit complicated when intermixing with plot_xy... */
	if (!x && colnr && !col_dwn && x_filled)
		curvenr--;
	if (curvenr >= ncurves)
		ReallocCurves(0);
	if (curvenr + 1 > ncurves_changed[2]) /* timer must recall DefineCurves in Form */
	{
		ncurves_changed[0] = 1;
		ncurves_changed[1] = ncurves_changed[2];
		ncurves_changed[2] = curvenr + 1;
	}

	/* If a new simulation, create new set of curves,
	   define identifiers, y axis... */
	if (rownr == 0)
	{
		if (FirstCallToUSER_GRAPH || new_ug)
		{
			if (colnr == 0)
			{
				prev_sim_no = simulation;
			}
			if (x)
			{ /* remove x_header... */
				for (i = curvenr; i < user_graph_count_headings - 1; i++)
					user_graph_headings[i] = user_graph_headings[i + 1];
				if (user_graph_count_headings > 0) user_graph_count_headings--;
				user_graph_headings =
					(char **) PHRQ_realloc(user_graph_headings,
								 (size_t) (user_graph_count_headings) * sizeof(char *));
				if (user_graph_headings == NULL && user_graph_count_headings > 0)
					malloc_error();
			}
		}

		if (!colnr &&(simulation != prev_sim_no || transport_step != prev_transport_step
			|| advection_step != prev_advection_step))
		{
			new_sim = true;
			if (!connect_simulations)
				AddSeries = TRUE;
		}
		prev_sim_no = simulation;

		if (new_sim && AddSeries && !connect_simulations)
		{ /* step to new curveset... */
			if (Curves[ncurves - 1].npoints)
				ReallocCurves(ncurves * 2);
			for (i = curvenr; i < ncurves; i++)
			{
				if (Curves[i].npoints)
					continue;
				else
				{ /* curve i is free... */
					ColumnOffset = curvenr = i;
					if (curvenr >= ncurves)
						ReallocCurves(0);
					ncurves_changed[0] = 1;
					ncurves_changed[1] = ncurves_changed[2];
					ncurves_changed[2] = curvenr + 1;
					break;
				}
			}
		}
		/* Or, add all to existing set... */
		else if (new_sim)
		{
			RowOffset = 1;
			for (i = 0; i < ncurves; i++)
				Curves[i].prev_npoints = Curves[i].npoints;
		}

		/* define curve identifier... */
		i = colnr + nCSV_headers;
		if (connect_simulations || new_ug)
			i = colnr + ColumnOffset;
		if (x_filled && !col_dwn)
			i--;
		if (!x && user_graph_count_headings > i)
		{
			PHRQ_free(Curves[curvenr].id);
			Curves[curvenr].id = string_duplicate(user_graph_headings[i]);
		}
		/* the y axis... */
		if (strcmp(a, "s") == 0)
			Curves[curvenr].y_axis = 2;
		/* Diamonds are small... */
		if (curvenr == 1)
			Curves[curvenr].symbol_size = Curves[curvenr - 1].symbol_size + (float) 1.0;

		if (nCSV_headers)
		{ /* use default properties of csv... */
			int cn;
			char *color[7] = {"Red", "Green", "Blue", "Orange", "Magenta", "Yellow", "Black" };
			for (i = 0; i < nCSV_headers; i++)
			{
				cn = i + ColumnOffset;
				if (cn >= ncurves)
						ReallocCurves(0);
				Curves[cn].symbol = string_hsave("None");
				Curves[cn].color = string_hsave(color[i]);
			}
		}
	}
	else if (rownr == 1)
		new_ug = false;


	/* return if the point is a zero... */
	if (!strlen(s))
	{
		if (x && colnr)
		{
			colnr--;
			col_dwn = true;
		}
		return;
	}

	/* define the x value for Curves(x, y)... */
	if (x)
	{
		x_value = (float) atof(s);

		if (!x_filled) /* Curves.x can be defined now... */
		{
			for (i = ColumnOffset; i < curvenr; i++)
			{
				if (y_filled[i - ColumnOffset])
				{
					Curves[i].x[Curves[i].npoints] = x_value;
					Curves[i].npoints++;
				}
			}
		}
		x_filled = true;
	}
	else /* the y value... */
	{
		if (Curves[curvenr].npoints >= Curves[curvenr].nxy)
			ReallocCurveXY(curvenr);
		Curves[curvenr].y[Curves[curvenr].npoints] = (float) atof(s);
		if (x_filled)
		{
			/* Add x_value */
			Curves[curvenr].x[Curves[curvenr].npoints] = x_value;
			Curves[curvenr].npoints++;
		}
		else
		{ /* x not yet defined, maybe later... */
			y_filled[curvenr - ColumnOffset] = true;
		}
	}
	if (!col_dwn && x_filled && colnr)
	{
		colnr--;
		col_dwn = true;
	}
	return;
}

void CLASS_QUALIFIER
PlotXY(char *x, char *y)
{
	/* Attribute values from *x and *y to Curves(*x, *y) */
	   
	int i, i2, i3;
	bool new_sim = false, new_trans = false;
	if ((state == TRANSPORT && transport_step != prev_transport_step) ||
		(state == ADVECTION && advection_step != prev_advection_step))
		new_trans = true;
	if (FirstCallToUSER_GRAPH && colnr == 0)
		prev_sim_no = simulation;
	else
		if (!rownr && (simulation != prev_sim_no || new_trans))
		{
			new_sim = true;
			if (!connect_simulations)
				AddSeries = TRUE;
		}
	prev_sim_no = simulation;

	int curvenr = colnr + ColumnOffset;

	if (curvenr >= ncurves)
		ReallocCurves(0);
	if (curvenr + 1 > ncurves_changed[2]) /* timer must recall DefineCurves in Form */
	{
		ncurves_changed[0] = 1;
		ncurves_changed[1] = ncurves_changed[2];
		ncurves_changed[2] = curvenr + 1;
	}
	if (x_filled && user_graph_count_headings > curvenr + ColumnOffset)
	{
		PHRQ_free(Curves[curvenr + ColumnOffset].id);
		Curves[curvenr + ColumnOffset].id =
			string_duplicate(user_graph_headings[curvenr + ColumnOffset]);
	}

	/* If a new simulation, create new set of curves,
	   define identifiers, y axis from values set in ExtractCurveInfo... */
	if (rownr == 0 && colnr == 0)
	{
		if (new_sim && AddSeries && (!connect_simulations || new_ug))
		{ /* step to new curveset... */
			if (Curves[ncurves - 1].npoints)
				ReallocCurves(ncurves * 2);
			for (i = curvenr; i < ncurves; i++)
			{
				if (Curves[i].npoints)
					continue;
				else
				{
				/* curve i is free... */
					i2 = i3 = ColumnOffset;
					ColumnOffset = curvenr = i;
					break;
				}
			}
			if (new_trans && !new_ug) i3 = 0;
			if (new_ug) i2 = 0;
			/* fill in curve properties... */
			for (i = ColumnOffset; i < ColumnOffset + (ColumnOffset - i2); i++)
			{
				if (i >= ncurves)
					ReallocCurves(0);
				/* define the new curve... */
				if (i3 < user_graph_count_headings)
				{
					PHRQ_free(Curves[i].id);
					Curves[i].id = string_duplicate(user_graph_headings[i3]);
				}
				//Curves[i].color = Curves[i3].color;
				//Curves[i].line_w = Curves[i3].line_w;
				//Curves[i].symbol = Curves[i3].symbol;
				//Curves[i].symbol_size = Curves[i3].symbol_size;
				Curves[i].y_axis = Curves[i3].y_axis;
				i3++;
			}
		}
		/* Or, add all to existing set... */
		else if (new_sim)
		{
			RowOffset = 1;
			for (i = 0; i < ncurves; i++) Curves[i].prev_npoints = Curves[i].npoints;
		}
		new_ug = false;
	}

	/* return if x or y is a zero... */
	if (!strlen(x) || !strlen(y)) return;

	/* fill in Curves(x, y)... */
	if (Curves[curvenr].npoints >= Curves[curvenr].nxy)
		ReallocCurveXY(curvenr);
	Curves[curvenr].x[Curves[curvenr].npoints] = (float) atof(x);
	Curves[curvenr].y[Curves[curvenr].npoints] = (float) atof(y);
	Curves[curvenr].npoints++;

	return;
}

void CLASS_QUALIFIER
start_chart(bool end)
{
	if (end)
		goto end_chart;
	Application::EnableVisualStyles();
	Application::SetCompatibleTextRenderingDefault(true); 

#if PHREEQC_CLASS
	Thread ^t = gcnew Thread(
                gcnew ParameterizedThreadStart(Form1::ThreadForm));
#else
	Thread ^t = gcnew Thread( gcnew ThreadStart( &Form1::ThreadForm));
#endif
	t->SetApartmentState(ApartmentState::STA);
	t->IsBackground = false;
	t->Priority = ThreadPriority::Normal;
#if PHREEQC_CLASS
	PhreeqcObj ^p = gcnew PhreeqcObj(this);
	t->Start(p);
#else
	t->Start();
#endif
	//Thread::Sleep( 1 ); /* this when debugging... */
	//_beginthread(void (Form1::ThreadForm), 0, NULL);
	return;

end_chart: /* wonder where this could be called... */
	t->Join();
	return;
}

#endif //zedgraph