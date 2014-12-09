#pragma once
#include <cassert>	
#ifdef PHREEQC_CLASS
//#define P_INSTANCE p_instance
//#define P_INSTANCE_COMMA p_instance,
#define P_INSTANCE_POINTER phreeqc_ptr->
//#define PHREEQC_PTR_ARG Phreeqc *p_instance
//#define PHREEQC_PTR_ARG_COMMA Phreeqc *p_instance,
#else
#define P_INSTANCE_POINTER
#endif
namespace zdg_ui2 {
	using namespace System;
	//using namespace System::ComponentModel;
	using namespace System::Resources;
	using namespace System::Windows::Forms;
	using namespace System::Drawing;
	using namespace System::Threading;
	using namespace ZedGraph;

#ifdef PHREEQC_CLASS
	public ref class PhreeqcObj : public System::Object
	{
	public: Phreeqc* phreeqc_ptr;
	public:	PhreeqcObj(Phreeqc* ptr)
	{
		this->phreeqc_ptr = ptr;
	}
	};
#endif

	public ref class Form1  : public System::Windows::Forms::Form
	{
	public:	long int tickStart;
	public: Form1 ^myForm;
	public:	Form1()
	{
#ifdef PHREEQC_CLASS
		this->phreeqc_ptr = NULL;
#endif
		InitializeComponent();
	}
#ifdef PHREEQC_CLASS
	public:	Form1(Phreeqc *ptr)
	{
		this->phreeqc_ptr = ptr;
		InitializeComponent();
	}
#endif
#ifdef PHREEQC_CLASS
	static void ThreadForm(Object^ data)
	{
		Phreeqc *ptr = ((PhreeqcObj^)(data))->phreeqc_ptr;
		ptr->u_g_active = true;
		Form1 ^myForm = gcnew Form1(ptr);
		myForm->ShowDialog();
		ptr->u_g_active = false;
	}
#else
	static void ThreadForm()
	{
		Form1 ^myForm = gcnew Form1();
		myForm->ShowDialog();
	}
#endif
	//void ThreadThis(Object^ data)
	//{
	//	this->ShowDialog();
	//	assert(false);
	//}
	private: void SetSize()
	{
		zg1->Location = Point( 0, 0 );
			// Leave a small margin around the outside of the control
		zg1->Size = System::Drawing::Size( ClientRectangle.Width - 0,
				ClientRectangle.Height - 0 );
	}

	System::Void Form1_Load(System::Object ^sender, System::EventArgs ^e)
	{
		CreateGraph( zg1 );
		SetSize();
	}

	System::Void Form1_Resize(System::Object ^sender, System::EventArgs ^e)
	{
		SetSize();
	}

	static bool LogX, LogY, LogY2;
	private: bool check_neg_log( int i, int i2)
		{
			if (LogX && P_INSTANCE_POINTER axis_scale_x[4] == 10.0 && P_INSTANCE_POINTER Curves[i].x[i2] <= 0)
			{
				P_INSTANCE_POINTER warning_msg("Obtained x_value <= 0, removing point...");
				//axis_scale_x[4] = NA; /* if reverting to linear... */
				//LogX = false;
				return true;
			}
			if (P_INSTANCE_POINTER Curves[i].y[i2] <= 0 && (P_INSTANCE_POINTER axis_scale_y[4] == 10.0 || P_INSTANCE_POINTER axis_scale_y2[4] == 10.0))
			{
				if (P_INSTANCE_POINTER Curves[i].y_axis == 2 && LogY2)
				{
					P_INSTANCE_POINTER warning_msg("Obtained sy_value <= 0, removing point......");
					//axis_scale_y2[4] = NA;
					//LogY2 = false;
				return true;
				}
				else if (LogY)
				{
					P_INSTANCE_POINTER warning_msg("Obtained y_value <= 0, removing point......");
					//axis_scale_y[4] = NA;
					//LogY = false;
				return true;
				}
			}
			return false;
		}

		private: PointPairList ^list;
		int col_use, symbol_use;
		bool Y2;
		static cli::array<String^> ^ColorList = {"Red", "Green", "Blue", "Orange", "Magenta", "Yellow", "Black" };

		void DefineCurves(GraphPane ^myPane, int init)
		{
			if (P_INSTANCE_POINTER ncurves_changed[0])
				P_INSTANCE_POINTER ncurves_changed[0] = 0;

			// Set the titles and axis labels
			myPane->Title->Text = gcnew String(P_INSTANCE_POINTER chart_title);
			myPane->XAxis->Title->Text = gcnew String(P_INSTANCE_POINTER axis_titles[0]);
			myPane->YAxis->Title->Text = gcnew String(P_INSTANCE_POINTER axis_titles[1]);
			myPane->Y2Axis->Title->Text = gcnew String(P_INSTANCE_POINTER axis_titles[2]);

			LineItem ^myCurve;

			Color col;

			//char *SymbolList[11] =  {"Square", "Diamond", "Triangle", "Circle", "XCross", "Plus", "Star",
			//	"TriangleDown", "HDash", "VDash", "None"};
			int s_symb;
			String ^s_t;
			if (P_INSTANCE_POINTER axis_scale_x[4] == 10.0) LogX = true;
			else LogX = false;
			if (P_INSTANCE_POINTER axis_scale_y[4] == 10.0) LogY = true;
			else LogY = false;
			if (P_INSTANCE_POINTER axis_scale_y2[4] == 10.0) LogY2 = true;
			else LogY2 = false;

			for (int i = init; i < P_INSTANCE_POINTER ncurves_changed[2]; i++)
			{
				if (P_INSTANCE_POINTER Curves[i].npoints == 0) continue;
				list = gcnew PointPairList();
				if (P_INSTANCE_POINTER Curves[i].y_axis == 2)
					Y2 = true;
				else
					Y2 = false;
				for (int i2 = 0; i2 < P_INSTANCE_POINTER Curves[i].npoints; i2++)
				{
					if ((LogX && P_INSTANCE_POINTER Curves[i].x[i2] <=0)
						|| (LogY && !Y2 && P_INSTANCE_POINTER Curves[i].y[i2] <=0)
						|| (LogY2 && Y2 && P_INSTANCE_POINTER Curves[i].y[i2] <=0))
						continue;
					else
						list->Add( P_INSTANCE_POINTER Curves[i].x[i2], P_INSTANCE_POINTER Curves[i].y[i2] );
				}
				if (strlen(P_INSTANCE_POINTER Curves[i].color) > 0) {
					col = Color::FromName(gcnew String(P_INSTANCE_POINTER Curves[i].color));
				}
				else col = Color::FromName(ColorList[col_use]);
				if (++col_use > 6) col_use = 0;
				
				if (strlen(P_INSTANCE_POINTER Curves[i].symbol) > 0)
				{
					int i2;
					for (i2 = 0; i2 < 11; i2++)
					{
						if (strncmp(P_INSTANCE_POINTER Curves[i].symbol, P_INSTANCE_POINTER SymbolList[i2], 2) == 0) break;
					}
					s_symb = i2;
				}
				else s_symb = symbol_use;
				if (++symbol_use > 10) symbol_use = 0;

				s_t = gcnew String(P_INSTANCE_POINTER Curves[i].id);

				switch (s_symb)
				{
					case 0:
					myCurve = myPane->AddCurve( s_t, list, col, SymbolType::Square );
						break;
					case 1:
						myCurve = myPane->AddCurve( s_t, list, col, SymbolType::Diamond );
						break;
					case 2:
						myCurve = myPane->AddCurve( s_t, list, col, SymbolType::Triangle );
						break;
					case 3:
						myCurve = myPane->AddCurve( s_t, list, col, SymbolType::Circle );
						break;
					case 4:
						myCurve = myPane->AddCurve( s_t, list, col, SymbolType::XCross );
						break;
					case 5:
						myCurve = myPane->AddCurve( s_t, list, col, SymbolType::Plus );
						break;
					case 6:
						myCurve = myPane->AddCurve( s_t, list, col, SymbolType::Star );
						break;
					case 7:
						myCurve = myPane->AddCurve( s_t, list, col, SymbolType::TriangleDown );
						break;
					case 8:
						myCurve = myPane->AddCurve( s_t, list, col, SymbolType::HDash );
						break;
					case 9:
						myCurve = myPane->AddCurve( s_t, list, col, SymbolType::VDash );
						break;
					case 10:
						myCurve = myPane->AddCurve( s_t, list, col, SymbolType::None );
						break;
					default:
						myCurve = myPane->AddCurve( s_t, list, col, SymbolType::Default );
						break;
				}
				if (P_INSTANCE_POINTER Curves[i].line_w > 0.0)
					myCurve->Line->Width = (float) P_INSTANCE_POINTER Curves[i].line_w;
				else
					myCurve->Line->IsVisible = false;
				/* hmm... dash/dot don't display well */
				//myCurve->Line->Style = System::Drawing::Drawing2D::DashStyle::Dot;
				myCurve->Symbol->Fill = gcnew Fill( Color::FromName("White") );
				if (P_INSTANCE_POINTER Curves[i].symbol_size > 0.0)
					myCurve->Symbol->Size = (float) P_INSTANCE_POINTER Curves[i].symbol_size;
				else
					myCurve->Symbol->IsVisible = false;
				myCurve->Symbol->Border->Width = (float) P_INSTANCE_POINTER Curves[i].line_w;
				if (Y2)
					myCurve->IsY2Axis = true;
				P_INSTANCE_POINTER Curves[i].npoints_plot = P_INSTANCE_POINTER Curves[i].npoints;
						
				delete list;
			}

			if (Y2)
				myPane->Legend->Position = ZedGraph::LegendPos::TopCenter;
			else
				myPane->Legend->Position = ZedGraph::LegendPos::Right;
			myPane->Legend->FontSpec->Size = 12;
			myPane->Legend->FontSpec->IsBold = false;

			// Show the x axis grid
			myPane->XAxis->MajorGrid->IsVisible = true;
			if (fabs(P_INSTANCE_POINTER axis_scale_x[0] - NA) > 1e-3)
				myPane->XAxis->Scale->Min = P_INSTANCE_POINTER axis_scale_x[0];
			else
				myPane->XAxis->Scale->MinAuto = true;
			if (fabs(P_INSTANCE_POINTER axis_scale_x[1] - NA) > 1e-3)
				myPane->XAxis->Scale->Max = P_INSTANCE_POINTER axis_scale_x[1];
			else
				myPane->XAxis->Scale->MaxAuto = true;
			if (fabs(P_INSTANCE_POINTER axis_scale_x[2] - NA) > 1e-3)
				myPane->XAxis->Scale->MajorStep = P_INSTANCE_POINTER axis_scale_x[2];
			else
				myPane->XAxis->Scale->MajorStepAuto = true;
			if (fabs(P_INSTANCE_POINTER axis_scale_x[3] - NA) > 1e-3)
			{
				myPane->XAxis->Scale->MinorStep = P_INSTANCE_POINTER axis_scale_x[3];
				if (P_INSTANCE_POINTER axis_scale_x[3] == 0.0)
				// remove minor tics
					myPane->XAxis->MinorTic->Size = 0;
			}
			else
				myPane->XAxis->Scale->MinorStepAuto = true;
			if (P_INSTANCE_POINTER axis_scale_x[4] == 10.0)
				myPane->XAxis->Type = AxisType::Log;

			// Make the Y axis scale red
			// myPane->YAxis->Scale->FontSpec->FontColor = Color::Red;
			// myPane->YAxis->Title->FontSpec->FontColor = Color::Red;
			// turn off the opposite tics so the Y tics don't show up on the Y2 axis
			if (Y2)
			{
				myPane->YAxis->MajorTic->IsOpposite = false;
				myPane->YAxis->MinorTic->IsOpposite = false;
			}
			// Don't display the Y zero line
			myPane->YAxis->MajorGrid->IsZeroLine = false;
			// Align the Y axis labels so they are flush to the axis
			myPane->YAxis->Scale->Align = AlignP::Inside;
			myPane->YAxis->MajorGrid->IsVisible = true;
			if (fabs(P_INSTANCE_POINTER axis_scale_y[0] - NA) > 1e-3)
				myPane->YAxis->Scale->Min = P_INSTANCE_POINTER axis_scale_y[0];
			else
				myPane->YAxis->Scale->MinAuto = true;
			if (fabs(P_INSTANCE_POINTER axis_scale_y[1] - NA) > 1e-3)
				myPane->YAxis->Scale->Max = P_INSTANCE_POINTER axis_scale_y[1];
			else
				myPane->YAxis->Scale->MaxAuto = true;
			if (fabs(P_INSTANCE_POINTER axis_scale_y[2] - NA) > 1e-3)
				myPane->YAxis->Scale->MajorStep = P_INSTANCE_POINTER axis_scale_y[2];
			else
				myPane->YAxis->Scale->MajorStepAuto = true;
			if (fabs(P_INSTANCE_POINTER axis_scale_y[3] - NA) > 1e-3)
			{
				myPane->YAxis->Scale->MinorStep = P_INSTANCE_POINTER axis_scale_y[3];
				if (P_INSTANCE_POINTER axis_scale_y[3] == 0.0)
				// remove minor tics
					myPane->YAxis->MinorTic->Size = 0;
			}
			else
				myPane->YAxis->Scale->MinorStepAuto = true;
			if (P_INSTANCE_POINTER axis_scale_y[4] == 10.0)
				myPane->YAxis->Type = AxisType::Log;

			// Enable the Y2 axis display
			if (Y2)
			{
				myPane->Y2Axis->IsVisible = true;
			// Make the Y2 axis scale blue
			// myPane->Y2Axis->Scale->FontSpec->FontColor = Color::Blue;
			// myPane->Y2Axis->Title->FontSpec->FontColor = Color::Blue;
			// turn off the opposite tics so the Y2 tics don't show up on the Y axis
				myPane->Y2Axis->MajorTic->IsOpposite = false;
				myPane->Y2Axis->MinorTic->IsOpposite = false;
			// Don't display the Y2 axis grid lines
				myPane->Y2Axis->MajorGrid->IsVisible = false;
			// Align the Y2 axis labels so they are flush to the axis
				myPane->Y2Axis->Scale->Align = AlignP::Inside;

				if (fabs(P_INSTANCE_POINTER axis_scale_y2[0] - NA) > 1e-3)
					myPane->Y2Axis->Scale->Min = P_INSTANCE_POINTER axis_scale_y2[0];
				else
					myPane->Y2Axis->Scale->MinAuto = true;
				if (fabs(P_INSTANCE_POINTER axis_scale_y2[1] - NA) > 1e-3)
					myPane->Y2Axis->Scale->Max = P_INSTANCE_POINTER axis_scale_y2[1];
				else
					myPane->Y2Axis->Scale->MaxAuto = true;
				if (fabs(P_INSTANCE_POINTER axis_scale_y2[2] - NA) > 1e-3)
					myPane->Y2Axis->Scale->MajorStep = P_INSTANCE_POINTER axis_scale_y2[2];
				else
					myPane->Y2Axis->Scale->MajorStepAuto = true;
				if (fabs(P_INSTANCE_POINTER axis_scale_y2[3] - NA) > 1e-3)
				{
					myPane->Y2Axis->Scale->MinorStep = P_INSTANCE_POINTER axis_scale_y2[3];
					if (P_INSTANCE_POINTER axis_scale_y2[3] == 0.0)
					// remove minor tics
						myPane->Y2Axis->MinorTic->Size = 0;
				}
				else
					myPane->Y2Axis->Scale->MinorStepAuto = true;
				if (P_INSTANCE_POINTER axis_scale_y2[4] == 10.0)
					myPane->Y2Axis->Type = AxisType::Log;
			}

			// Fill the axis background with a gradient
			//myPane->Chart->Fill = gcnew Fill( Color::White, Color::LightYellow, 45.0f ); /* FromArgb(255, 255, 224) */
			myPane->Chart->Fill = gcnew Fill( Color::White, Color::FromArgb(255, 255, 230), 45.0f );
		}

		public: void CreateGraph( ZedGraphControl ^z1 )	{
			// Get a reference to the GraphPane instance in the ZedGraphControl
			GraphPane ^myPane = z1->GraphPane;

			DefineCurves(myPane, 0);

			// Add text boxes with instructions...
			TextObj ^text;
			text = gcnew TextObj(
				L" Click right mouse for options...",
				0.01f, 0.99f, CoordType::PaneFraction, AlignH::Left, AlignV::Bottom );
			text->FontSpec->StringAlignment = StringAlignment::Near;
			text->FontSpec->Size = 10;
			text->FontSpec->FontColor = Color::Red;
			myPane->GraphObjList->Add( text );
			text = gcnew TextObj(
				L" Press Alt + F4 to quit",
				0.81f, 0.99f, CoordType::PaneFraction, AlignH::Left, AlignV::Bottom );
			text->FontSpec->StringAlignment = StringAlignment::Near;
			text->FontSpec->Size = 10;
			text->FontSpec->FontColor = Color::Red;
			myPane->GraphObjList->Add( text );

			// Enable scrollbars if needed...
			/*z1->IsShowHScrollBar = true;
			z1->IsShowVScrollBar = true;
			z1->IsAutoScrollRange = true;
			z1->IsScrollY2 = true;*/

			// OPTIONAL: Show tooltips when the mouse hovers over a point
			z1->IsShowPointValues = false;
			z1->PointValueEvent += gcnew ZedGraphControl::PointValueHandler( this,
					&Form1::MyPointValueHandler );

			// OPTIONAL: Add a custom context menu item
			z1->ContextMenuBuilder += gcnew	ZedGraphControl::ContextMenuBuilderEventHandler(
					this, &Form1::MyContextMenuBuilder );

			// OPTIONAL: Handle the Zoom Event
			z1->ZoomEvent += gcnew ZedGraphControl::ZoomEventHandler( this,
						&Form1::MyZoomEvent );

			// Size the control to fit the window
			SetSize();

			// Tell ZedGraph to calculate the axis ranges
			// Note that you MUST call this after enabling IsAutoScrollRange, since AxisChange() sets
			// up the proper scrolling parameters
			
			z1->AxisChange();
			// Make sure the Graph gets redrawn
			z1->Invalidate();
			timer1->Interval = P_INSTANCE_POINTER update_time_chart;
			timer1->Enabled = true;
			timer1->Start();

			tickStart = Environment::TickCount;
		}

		/// <summary>
		/// Display customized tooltips when the mouse hovers over a point
		/// </summary>
		System::String ^MyPointValueHandler( ZedGraphControl ^control, GraphPane ^pane,
						CurveItem ^curve, int iPt ) {
			// Get the PointPair that is under the mouse
			PointPair pt = curve[iPt];
			return curve->Label->Text + " is " + pt.Y.ToString( "f3" ) + " units at X = " + pt.X.ToString( "f3" );
		}

		// Add some explanation to the menu..
		void MyContextMenuBuilder( ZedGraphControl ^control,
					System::Windows::Forms::ContextMenuStrip ^menuStrip,
					Point mousePt,
					ZedGraphControl::ContextMenuObjectState objState ) {
			ToolStripMenuItem ^item = gcnew ToolStripMenuItem();
			item->Text = L"Zoom: left mouse + drag\nPan: middle mouse + drag";
			menuStrip->Items->Insert(5, item );

			menuStrip->Items->RemoveAt(0);
			ToolStripMenuItem ^item2 = gcnew ToolStripMenuItem();
			item2->Text = L"Save Data to File \'curves.u_g\'";
			item2->Click += gcnew System::EventHandler(this, &zdg_ui2::Form1::SaveCurves );
			menuStrip->Items->Insert(0, item2 );

		}
		void SaveCurves( System::Object ^sender, System::EventArgs ^e )
		{
			P_INSTANCE_POINTER SaveCurvesToFile("curves.u_g");
		}

		// Respond to a Zoom Event
		void MyZoomEvent( ZedGraphControl ^control, ZoomState ^oldState, ZoomState ^newState )
		{
			// Here we get notification everytime the user zooms
		}

		// update the chart with new data...
		private: void timer1_Tick(System::Object ^sender, System::EventArgs ^e )
		{
			LineItem  ^curve;
			if ( (Environment::TickCount - tickStart ) > P_INSTANCE_POINTER update_time_chart) {
				P_INSTANCE_POINTER all_points = true;
				if (P_INSTANCE_POINTER ncurves_changed[0])
				{
					DefineCurves(zg1->GraphPane, zg1->GraphPane->CurveList->Count);
					P_INSTANCE_POINTER all_points = false;
				}
				else
			// Get the graph curves...
				for (int i = 0; i < zg1->GraphPane->CurveList->Count; i++) {
					curve =  (LineItem ^) zg1->GraphPane->CurveList[i];
					 // Get the PointPairList
					IPointListEdit  ^ip = (IPointListEdit^) curve->Points;
					if (P_INSTANCE_POINTER Curves[i].npoints_plot != P_INSTANCE_POINTER Curves[i].npoints)
						P_INSTANCE_POINTER all_points = false;
					else
						P_INSTANCE_POINTER all_points = true;

					for ( int i2 = P_INSTANCE_POINTER Curves[i].npoints_plot; i2 < P_INSTANCE_POINTER Curves[i].npoints; i2++ )
					{
						if ((LogX || LogY || LogY2) && (P_INSTANCE_POINTER Curves[i].x[i2] <=0
							|| P_INSTANCE_POINTER Curves[i].y[i2] <=0))
							continue;
						else
							ip->Add(P_INSTANCE_POINTER Curves[i].x[i2], P_INSTANCE_POINTER Curves[i].y[i2] );
					}
					P_INSTANCE_POINTER Curves[i].npoints_plot = P_INSTANCE_POINTER Curves[i].npoints;
				}
				/* explicitly reset the max in case of log scale, zedgraphs doesn't do this... */
				if ((fabs(P_INSTANCE_POINTER axis_scale_x[1] - NA) < 1e-3) && zg1->GraphPane->XAxis->Type == AxisType::Log)
				{
					double max = -1e99;
					for  (int i = 0; i < zg1->GraphPane->CurveList->Count; i++)
					{
						if (P_INSTANCE_POINTER Curves[i].x[P_INSTANCE_POINTER Curves[i].npoints - 1] > max)
							max = P_INSTANCE_POINTER Curves[i].x[P_INSTANCE_POINTER Curves[i].npoints - 1];
					}
					max += pow(10.0, log10(max / 3));
					zg1->GraphPane->XAxis->Scale->Max = max;
				}
				if ((fabs(P_INSTANCE_POINTER axis_scale_y[1] - NA) < 1e-3) && zg1->GraphPane->YAxis->Type == AxisType::Log)
				{
					double max = -1e99;
					for  (int i = 0; i < zg1->GraphPane->CurveList->Count; i++)
					{
						curve =  (LineItem ^) zg1->GraphPane->CurveList[i];
						if (curve->IsY2Axis) continue;
						if (P_INSTANCE_POINTER Curves[i].y[P_INSTANCE_POINTER Curves[i].npoints - 1] > max)
							max = P_INSTANCE_POINTER Curves[i].y[P_INSTANCE_POINTER Curves[i].npoints - 1];
					}
					max += pow(10.0, log10(max / 3));
					zg1->GraphPane->YAxis->Scale->Max = max;
				}
				if ((fabs(P_INSTANCE_POINTER axis_scale_y2[1] - NA) < 1e-3) && zg1->GraphPane->Y2Axis->Type == AxisType::Log)
				{
					double max = -1e99;
					for  (int i = 0; i < zg1->GraphPane->CurveList->Count; i++)
					{
						curve =  (LineItem ^) zg1->GraphPane->CurveList[i];
						if (!curve->IsY2Axis) continue;
						if (P_INSTANCE_POINTER Curves[i].y[P_INSTANCE_POINTER Curves[i].npoints - 1] > max)
							max = P_INSTANCE_POINTER Curves[i].y[P_INSTANCE_POINTER Curves[i].npoints - 1];
					}
					max += pow(10.0, log10(max / 3));
					zg1->GraphPane->Y2Axis->Scale->Max = max;
				}

				zg1->AxisChange();
				zg1->Refresh();
				tickStart = Environment::TickCount;
			}

			if (P_INSTANCE_POINTER end_timer && P_INSTANCE_POINTER all_points)
			{
				timer1->Stop();
				//SaveCurvesToFile("c:\\temp\\cv.ug1");
			}
			return;
		}

		~Form1() {
			if (this->zg1) delete zg1;
			//if (this->timer1) delete timer1);
			if (components) {
				delete components;
			}
			P_INSTANCE_POINTER DeleteCurves(); /* perhaps not even needed... */
		}
		public: ZedGraph::ZedGraphControl ^zg1;
		private: System::Windows::Forms::Timer ^timer1;
		private: System::ComponentModel::Container ^components;
#ifdef PHREEQC_CLASS
		private: Phreeqc * phreeqc_ptr;
#endif

public:
#pragma region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		void InitializeComponent()
		{
			this->components = (gcnew System::ComponentModel::Container());
			this->zg1 = (gcnew ZedGraph::ZedGraphControl());
			this->timer1 = (gcnew System::Windows::Forms::Timer( this->components ));
			this->SuspendLayout();
			// 
			// zg1
			// 
			this->zg1->Location = System::Drawing::Point(12, 12);
			this->zg1->Name = L"zg1";
			this->zg1->ScrollGrace = 0;
			this->zg1->ScrollMaxX = 0;
			this->zg1->ScrollMaxY = 0;
			this->zg1->ScrollMaxY2 = 0;
			this->zg1->ScrollMinX = 0;
			this->zg1->ScrollMinY = 0;
			this->zg1->ScrollMinY2 = 0;
			this->zg1->Size = System::Drawing::Size(P_INSTANCE_POINTER PanelWidth - 2 * 12, P_INSTANCE_POINTER PanelHeight - 2 * 12);
			this->zg1->TabIndex = 0;
			this->timer1->Tick += gcnew System::EventHandler( this, &Form1::timer1_Tick );
			// 
			// Form1
			// 
			this->AutoScaleDimensions = System::Drawing::SizeF(6, 13);
			this->AutoScaleMode = System::Windows::Forms::AutoScaleMode::Font;
			this->AutoValidate = System::Windows::Forms::AutoValidate::EnablePreventFocusChange;
			this->ClientSize = System::Drawing::Size(P_INSTANCE_POINTER PanelWidth, P_INSTANCE_POINTER PanelHeight);
			this->Controls->Add(this->zg1);
			this->Name = L"Form1";
			this->StartPosition = System::Windows::Forms::FormStartPosition::WindowsDefaultLocation;//:CenterScreen;
			this->Text = L"PHREEQC chart";
			this->TopMost = true;
			System::ComponentModel::ComponentResourceManager^  resources = (gcnew System::ComponentModel::ComponentResourceManager(Form1::typeid));
			try
			{
				this->Icon = (cli::safe_cast<System::Drawing::Icon^  >(resources->GetObject(L"$this.Icon")));
			}
			catch (...)
			{
			}

			this->Load += gcnew System::EventHandler(this, &Form1::Form1_Load);
			this->Resize += gcnew System::EventHandler(this, &Form1::Form1_Resize);
			this->ResumeLayout(false);
		}
#pragma endregion
	

};
}