//import java.io._
//import java.net._
//import java.awt._
//import java.awt.event._
//import javax.swing._
//import javax.swing.border._
//import javax.swing.event._
//import java.util._
//
//import att.grappa._
//
//class Grappa {
//
//    val INSTANCES = "instances"
//    val SCRIPT = "formatDemo"
//
//    val frame : DemoFrame  = null
//    var graph : Graph = null
//
//    /*
//     * simple-minded example that builds the graph:
//     *   args[0] -> args[1] -> args[2] -> ... -> args[args.length - 1]
//     */
//
//    def main(args:Array[String] ): Unit = {
//      val sb = new Grappa()
//      sb.load(args)
//      sb.display()
//    }
//
//  Element.setUserAttributeType(INSTANCES, GrappaConstants.INTEGER_TYPE)
//
//  def load(nodes:Array[String]): Unit = {
//
//  //int     inst, prev, crnt, cnt;
//  //val eg: Edge     = null
//  val nd = Array[Node](null,null)
//  //String  name
//
//  if (graph == null) {
//      graph = new Graph("Grappa"/*, true, false*/);
//  } else {
//      graph.reset(/*"Grappa", true, false*/);
//  }
//  graph.setMenuable(true);
//
//  graph.setAttribute("rankdir","LR");
//  graph.setAttribute("label","\\n\\n \\n \\n \\n");
//  graph.setAttribute("fontstyle","bold");
//  graph.setAttribute("fontsize","24");
//  graph.setAttribute("font","Helvetica");
//
//  graph.setNodeAttribute("shape","ellipse");
//  graph.setNodeAttribute("style","filled");
//  graph.setNodeAttribute("color","beige");
//  graph.setNodeAttribute("tip","A Node");
//
//  graph.setEdgeAttribute("color","darkgreen");
//  graph.setEdgeAttribute("tip","An Edge");
//  graph.setEdgeAttribute(INSTANCES,"1");
//
//  if (nodes != null) {
//    for (cnt <- 0 until nodes.length) {
//      crnt = cnt % 2
//      prev = (crnt+1) % 2
//      if (nodes(cnt) == null || nodes(cnt).length == 0) {
//          name = "<no_name>"
//      } else {
//          name = nodes(cnt)
//      }
//      
//      nd(crnt) = graph.findNodeByName(name)
//      if (nd(crnt) == null) {
//        nd(crnt) = new Node(graph, name)
//        /*
//         * set the initial position just so they don't all draw at (0,0). The display
//         * display frame has a button that takes advantage of GrappaSupport.filterGraph(...)
//         * to get a better layout by letting one of the graphviz layout tools
//         * such as "dot" or "neato" do the layout for you.
//         */
//        nd(crnt).setAttribute(GrappaConstants.POS_ATTR, ((cnt*125) + ",0"))
//    }
//    if (nd(prev) != null) {
//        name = nd(prev).getName() + "->" + name
//        eg = graph.findEdgeByName(name)
//        if (eg == null) {
//          eg = new Edge(graph, nd(prev), nd(crnt), name)
//        } else {
//          inst = eg.getAttributeValue(INSTANCES).asInstanceOf[Int] + 1
//          eg.setAttribute(INSTANCES, inst)
//        }
//    }
//      }
//  }
//    }
//
//    def display() {
//
//  frame = new DemoFrame()
//  frame.show()
//    }
//
//    class DemoFrame extends JFrame with ActionListener
//    {
//      //GrappaPanel gp;
//      //JScrollPane jsp;
//    
//      val layout : JButton= null;
//      val printer : JButton= null;
//      val quit : JButton= null;
//      val panel : JButton= null;
//  
//  public DemoFrame() {
//      super("DemoFrame");
//
//      setSize(600,400);
//      setLocation(100,100);
//
//      addWindowListener(new WindowAdapter() {
//        public void windowClosing(WindowEvent wev) {
//      Window w = wev.getWindow();
//      w.setVisible(false);
//      w.dispose();
//      System.exit(0);
//        }
//    });
//
//      jsp = new JScrollPane();
//      // BLIT_SCROLL_MODE is nicer except zooming causes problems;
//      // need to investigate that a bit someday
//      //jsp.getViewport().setScrollMode(JViewport.BLIT_SCROLL_MODE);
//      jsp.getViewport().setScrollMode(JViewport.BACKINGSTORE_SCROLL_MODE);
//      jsp.getViewport().setBackground(Color.green);
//
//      gp = new GrappaPanel(graph);
//      gp.addGrappaListener(new GrappaAdapter());
//      gp.setScaleToFit(false);
//
//      java.awt.Rectangle bbox = graph.getBoundingBox().getBounds();
//  
//      GridBagLayout gbl = new GridBagLayout();
//      GridBagConstraints gbc = new GridBagConstraints();
//
//      gbc.gridwidth = GridBagConstraints.REMAINDER;
//      gbc.fill = GridBagConstraints.HORIZONTAL;
//      gbc.anchor = GridBagConstraints.NORTHWEST;
//
//      panel = new JPanel();
//      panel.setLayout(gbl);
//
//      layout = new JButton("Layout");
//      gbl.setConstraints(layout,gbc);
//      panel.add(layout);
//      layout.addActionListener(this);
//
//      printer = new JButton("Print");
//      gbl.setConstraints(printer,gbc);
//      panel.add(printer);
//      printer.addActionListener(this);
//
//      quit = new JButton("Quit");
//      gbl.setConstraints(quit,gbc);
//      panel.add(quit);
//      quit.addActionListener(this);
//
//      getContentPane().add("Center", jsp);
//      getContentPane().add("West", panel);
//
//      setVisible(true);
//      jsp.setViewportView(gp);
//  }
//
//  def actionPerformed( evt:ActionEvent) {
//      if(evt.getSource().isInstanceOf[JButton]) {
//    val tgt : JButton= evt.getSource().asInstanceOf[JButton]
//    if(tgt == quit) {
//        System.exit(0)
//    } else if(tgt == printer) {
//        graph.printGraph(System.out);
//        System.out.flush();
//    } else if(tgt == layout) {
//        var connector: Object =
//        try {
//          Runtime.getRuntime().exec(Grappa.SCRIPT)
//        } catch{ case ex: Exception =>
//          System.err.println("Exception while setting up Process: " + ex.getMessage() + "\nTrying URLConnection...")
//          null
//        }
//        
//        if(connector == null) {
//      try {
//          connector = (new URL("http://www.research.att.com/~john/cgi-bin/format-graph")).openConnection()
//          val urlConn : URLConnection = connector.asInstanceOf[URLConnection]
//          urlConn.setDoInput(true);
//          urlConn.setDoOutput(true);
//          urlConn.setUseCaches(false);
//          urlConn.setRequestProperty("Content-Type","application/x-www-form-urlencoded");
//      } catch {
//        case ex: Exception =>
//          System.err.println("Exception while setting up URLConnection: " + ex.getMessage() + "\nLayout not performed.");
//          connector = null;
//      }
//        }
//        if(connector != null) {
//      if(!GrappaSupport.filterGraph(graph,connector)) {
//          System.err.println("ERROR: somewhere in filterGraph");
//      }
//      if(connector .isInstanceOf[ Process]) {
//          try {
//            val code : Int = (connector.asInstanceOf[Process]).waitFor()
//            if(code != 0) {
//                System.err.println("WARNING: proc exit code is: " + code);
//            }
//          } catch { case ex: InterruptedException =>
//            System.err.println("Exception while closing down proc: " + ex.getMessage());
//            ex.printStackTrace(System.err);
//          }
//      }
//      connector = null;
//        }
//        graph.repaint();
//    }
//      }
//  }
//    }
//}
