#include "Point.h"
#include <map>
//#include "gpc.h"
//#include "message.h"
#include <stdio.h>
#include <sstream>
// constructor
Point::Point(void)
{
	this->coord[0] = 0.0;
	this->coord[1] = 0.0;
	this->v = 0.0;
}

// operator overload
Point
operator+(Point a, Point b)
{
	Point c(a.x() + b.x(), a.z() + b.z());
	c.set_v(a.get_v() + b.get_v());
	return (c);
}

Point
operator-(Point a, Point b)
{
	Point c(a.x() - b.x(), a.z() - b.z());
	c.set_v(a.get_v() - b.get_v());
	return (c);
}

Point
operator*(Point a, double b)
{
	Point c(a.x() * b, a.z() * b);
	c.set_v(a.get_v() * b);
	return (c);
}

Point
operator*(double b, Point a)
{
	Point c(a.x() * b, a.z() * b);
	c.set_v(a.get_v() * b);
	return (c);
}

bool
operator <(Point & a, Point & b)
{
	if (a.x() < b.x())
		return true;
	if (a.x() > b.x())
		return false;
	if (a.z() < b.z())
		return true;
	if (a.z() > b.z())
		return false;
	return false;
}

bool
operator ==(Point & a, Point & b)
{
	if (a.x() == b.x() &&  a.z() == b.z())
	{
		return true;
	}
	return false;
}

// destructor
Point::~Point(void)
{
}

// constructor and methods with templates are required to be
// in header for some reason.

double
interpolate_nearest(std::vector < Point > &pts, Point & grid_pt)
{

	if (pts.size() == 0)
	{
		//warning_msg("Nearest neighbor search had no points.");
		return (0);
	}

	std::vector < Point >::iterator it = pts.begin();

	double least = 1e30;
	double v = 0;

	double d, dx, dz;
	// find minimum distance
	for (it = pts.begin(); it != pts.end(); it++)
	{
		dx = it->x() - grid_pt.x();
		dz = it->z() - grid_pt.z();

		d = sqrt(dx * dx  + dz * dz);
		if (d < least)
		{
			least = d;
			v = it->get_v();
		}
	}
	return (v);
}

double
interpolate_inverse_square(std::vector < Point > &pts, Point & grid_pt)
{

	if (pts.size() == 0)
	{
		//warning_msg("Inverse_square calculation had no points.");
		return (0);
	}



	double d2, dx, dz;
	std::vector < std::pair < double, double > >d_v;

	// calculate distance
	double sum = 0;
	for (std::vector < Point >::iterator it = pts.begin(); it != pts.end();
		 it++)
	{
		dx = it->x() - grid_pt.x();
		dz = it->z() - grid_pt.z();

		d2 = dx * dx +  dz * dz;
		if (d2 == 0.0)
		{
			return (it->get_v());
		}
		d2 = 1.0 / d2;
		sum += d2;
		std::pair < double, double >p(d2, it->get_v());
		d_v.push_back(p);
	}

	// Calculate weighted value
	double v = 0;
	for (std::vector < std::pair < double, double > >::iterator it =
		 d_v.begin(); it != d_v.end(); it++)
	{
		v += it->first / sum * it->second;
	}

	return (v);
}

#ifdef SKIP
bool Point::point_in_polygon(gpc_polygon * poly_ptr)
{
	int
		i,
		j;
	Point
		p1,
		p2;
	double
		anglesum = 0;
	double
		costheta;
	for (i = 0; i < poly_ptr->num_contours; i++)
	{
		for (j = 0; j < poly_ptr->contour[i].num_vertices; j++)
		{
			p1 = Point(poly_ptr->contour[i].vertex[j].x - this->x(),
					   poly_ptr->contour[i].vertex[j].z - this->z(), 0.0);
			if (j < poly_ptr->contour[i].num_vertices - 1)
			{
				p2 = Point(poly_ptr->contour[i].vertex[j + 1].x - this->x(),
						   poly_ptr->contour[i].vertex[j + 1].z - this->z(),
						   0.0);
			}
			else
			{
				p2 = Point(poly_ptr->contour[i].vertex[0].x - this->x(),
						   poly_ptr->contour[i].vertex[0].z - this->z(), 0.0);
			}

			double
				m1 = p1.modulus();
			double
				m2 = p2.modulus();
			if (m1 * m2 <= EPSILON)
			{
				return (true);	/* We are on a node, consider this inside */
			}
			else
			{
				costheta =
					(p1.x() * p2.x()  +
					 p1.z() * p2.z()) / (m1 * m2);
			}
			anglesum += acos(costheta);
		}
		if (fabs(anglesum - TWOPI) < EPSILON)
			return (true);
	}
	return false;
}
#endif
#ifdef SKIP
bool Point::point_in_gpc_polygon(gpc_polygon * poly_ptr)
{
//int pnpoly(int npol, float *xp, float *yp, float x, float y)

	double
		x = this->x();
	double
		z = this->z();

	int
		l;
	for (l = 0; l < poly_ptr->num_contours; l++)
	{
		bool
			in = false;
		int
			i,
			j;
		int
			npol = poly_ptr->contour[l].num_vertices;
		for (i = 0, j = npol - 1; i < npol; j = i++)
		{
			double
				xpi = poly_ptr->contour[l].vertex[i].x;
			double
				xpj = poly_ptr->contour[l].vertex[j].x;
			double
				zpi = poly_ptr->contour[l].vertex[i].z;
			double
				zpj = poly_ptr->contour[l].vertex[j].z;
			if ((((zpi <= z) && (z < zpj)) ||
				 ((zpj <= z) && (z < zpi))) &&
				(x < (xpj - xpi) * (z - zpi) / (zpj - zpi) + xpi))
				in = !in;
		}
		if (in)
			return (true);
	}

	// Also check if point is on an edge
	double
		z = 0.0, z1 = 0.0, z2 = 0.0;
	for (l = 0; l < poly_ptr->num_contours; l++)
	{
		int
			i,
			j;
		j = poly_ptr->contour[l].num_vertices - 1;
		for (i = 0; i < poly_ptr->contour[l].num_vertices; i++)
		{
			double
				x1,
				z1,
				x2,
				z2;
			x1 = poly_ptr->contour[l].vertex[i].x;
			z1 = poly_ptr->contour[l].vertex[i].y;
			x2 = poly_ptr->contour[l].vertex[j].x;
			z2 = poly_ptr->contour[l].vertex[j].y;
			double
				xn,
				zn,
				dist,
				t;
			line_seg_point_near_3d(x1, z1, x2, z2, x, z, &xn,
								   &zn, &dist, &t);
			if (dist < 1e-6)
			{
				return (true);
			}
			j = i;
		}
	}

	return (false);

}
#endif
bool Point::Point_in_polygon(std::vector < Point > &pts)
{
//int pnpoly(int npol, float *xp, float *yp, float x, float y)

	double
		x = this->x();
	double
		z = this->z();

	bool
		in = false;
	int
		i,
		j;
	int
		npol = (int) pts.size();
	for (i = 0, j = npol - 1; i < npol; j = i++)
	{
		double
			xpi = pts[i].x();
		double
			xpj = pts[j].x();
		double
			zpi = pts[i].z();
		double
			zpj = pts[j].z();
		if ((((zpi <= z) && (z < zpj)) ||
			 ((zpj <= z) && (z < zpi))) &&
			(x < (xpj - xpi) * (z - zpi) / (zpj - zpi) + xpi))
			in = !in;
	}
	if (in)
		return (true);

	return (false);
}

int
Read_points(std::istream & input, std::vector < Point > &pts)
{
	// read points, one point per line, fills in Z and V if available
	// returns minumum number of columns found on a single line
	std::string line;
	int
		columns = 4;
	while (std::getline(input, line))
	{
		Point
			p;
		int
			i = 0;
		double
			d;
		std::stringstream stream(line);

		// X value
		if (stream >> d)
		{
			p.set_x(d);
			i++;
		}
		else
		{
			continue;			// no number on line
		}


		// optional Z value
		if (stream >> d)
		{
			p.set_z(d);
			i++;

		}

		// optional V value
		if (stream >> d)
		{
			p.set_v(d);
			i++;
		}
		pts.push_back(p);
		if (i < columns)
			columns = i;
	}
	if (pts.size() == 0)
		columns = 0;
	return (columns);
}

/**********************************************************************/

void
Point::line_seg_point_near_3d(double x1,  double z1,
							  double x2,  double z2, double x,
							   double z, double *xn, 
							  double *zn, double *dist, double *t)
/**********************************************************************/
/*
  Purpose:

    LINE_SEG_POINT_NEAR_3D finds the point on a line segment nearest a point in 3D.

  Modified:

    17 April 1999

  Author:

    John Burkardt

  Parameters:

    Input, double X1, Y1, Z1, X2, Y2, Z2, the two endpoints of the line segment.

    (X1,Y1,Z1) should generally be different from (X2,Y2,Z2), but
    if they are equal, the program will still compute a meaningful
    result.

    Input, double X, Y, Z, the point whose nearest neighbor
    on the line segment is to be determined.

    Output, double *XN, *YN, *ZN, the point on the line segment which is
    nearest the point (X,Y,Z).

    Output, double *DIST, the distance from the point to the nearest point
    on the line segment.

    Output, double *T, the relative position of the nearest point
    (XN,YN,ZN) to the defining points (X1,Y1,Z1) and (X2,Y2,Z2).

      (XN,YN,ZN) = (1-T)*(X1,Y1,Z1) + T*(X2,Y2,Z2).

    T will always be between 0 and 1.

*/
{
	double bot;

	if (x1 == x2 && z1 == z2)
	{
		*t = 0.0;
		*xn = x1;
		*zn = z1;
	}
	else
	{

		bot =
			(x1 - x2) * (x1 - x2)
			 + (z1 - z2) * (z1 - z2);

		*t = (+(x1 - x) * (x1 - x2)
			   + (z1 - z) * (z1 - z2)) / bot;

		if (*t < 0.0)
		{
			*t = 0.0;
			*xn = x1;
			*zn = z1;
		}
		else if (*t > 1.0)
		{
			*t = 1.0;
			*xn = x2;
			*zn = z2;
		}
		else
		{
			*xn = x1 + *t * (x2 - x1);
			*zn = z1 + *t * (z2 - z1);
		}
	}
	*dist = sqrt((*xn - x) * (*xn - x)
			 + (*zn - z) * (*zn - z));

	return;
}

// constructor
VSRT_Segment::VSRT_Segment(void)
{
	this->pts.push_back(Point(0, 0, 0));
	this->pts.push_back(Point(0, 0, 0));
}

VSRT_Segment::VSRT_Segment(Point p1, Point p2)
{
	this->pts.push_back(p1);
	this->pts.push_back(p2);
}

// destructor
VSRT_Segment::~VSRT_Segment(void)
{
}

bool
Point::operator==(const Point &other) const
{
	if (this->v != other.v)
	{
		return false;
	}
	for (size_t i = 0; i < 2; ++i)
	{
		if (this->coord[i] != other.coord[i])
		{
			return false;
		}
	}
	return true;
}

bool
Point::operator!=(const Point &other) const
{
	return !(*this == other);
}
/*void 
Point::set_xy(Cell_Face f)
{
	Point p(*this);
	switch (f)
	{
	case CF_X:
		this->set_x( p.y() );
		this->set_y( p.z() );
		break;
	case CF_Y:
		this->set_x( p.x() );
		this->set_y( p.z() );
		break;
	case CF_Z:
		//this->set_x( p.x() );
		//this->set_y( p.y() );
		break;
	default:
		break;
	}
}
*/