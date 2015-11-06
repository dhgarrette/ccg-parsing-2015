//package dhg.ccg.parse.dep;
//
///*___INFO__MARK_BEGIN__*/
///*************************************************************************
// *
// *  The Contents of this file are made available subject to the terms of
// *  the Sun Industry Standards Source License Version 1.2
// *
// *  Sun Microsystems Inc., March, 2001
// *
// *
// *  Sun Industry Standards Source License Version 1.2
// *  =================================================
// *  The contents of this file are subject to the Sun Industry Standards
// *  Source License Version 1.2 (the "License"); You may not use this file
// *  except in compliance with the License. You may obtain a copy of the
// *  License at http://gridengine.sunsource.net/Gridengine_SISSL_license.html
// *
// *  Software provided under this License is provided on an "AS IS" basis,
// *  WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING,
// *  WITHOUT LIMITATION, WARRANTIES THAT THE SOFTWARE IS FREE OF DEFECTS,
// *  MERCHANTABLE, FIT FOR A PARTICULAR PURPOSE, OR NON-INFRINGING.
// *  See the License for the specific provisions governing your rights and
// *  obligations concerning the Software.
// *
// *   The Initial Developer of the Original Code is: Sun Microsystems, Inc.
// *
// *   Copyright: 2006 by Sun Microsystems, Inc
// *
// *   All Rights Reserved.
// *
// ************************************************************************/
///*___INFO__MARK_END__*/
//import com.sun.grid.grm.GrmException;
//import com.sun.grid.grm.sdmmon.node.ResourceNode;
//import com.sun.grid.grm.sdmmon.node.ResourceProviderNode;
//import com.sun.grid.grm.sdmmon.node.RootNode;
//import com.sun.grid.grm.sdmmon.node.ServiceNode;
//import com.sun.grid.grm.sdmmon.scene.SdmScene;
//import org.netbeans.api.visual.graph.layout.GraphLayout;
//import org.netbeans.api.visual.graph.layout.UniversalGraph;
//import org.netbeans.api.visual.widget.Widget;
//
//import java.awt.*;
//import java.util.*;
//import java.util.logging.Level;
//import java.util.logging.Logger;
//import org.openide.nodes.Node;
//
///**
// * Performs layout of SdmScene.
// */
//public final class SdmSceneLayout extends GraphLayout<Node, Node> {
//
//    private int verticalGap;
//    private int horizontalGap;
//    private int serviceGap;
//    private int resourceGap;
//    private final static double DEFAULT_SVC_RADIUS = 200;
//    private final static double DEFAULT_RES_RADIUS = 100;
//
//    public SdmSceneLayout(int verticalGap, int horizontalGap) {
//        this.verticalGap = verticalGap;
//        this.horizontalGap = horizontalGap;
//    }
//
//    public void setProperties(int verticalGap, int horizontalGap) {
//        this.verticalGap = verticalGap;
//        this.horizontalGap = horizontalGap;
//    }
//
//    protected void performGraphLayout(UniversalGraph<Node, Node> graph) {
//        SdmScene scene;
//        /* sdmscene layout works just for SdmScene */
//        if (!(graph.getScene() instanceof SdmScene)) {
//            return;
//        } else {
//            scene = (SdmScene) graph.getScene();
//        }
//
//        RootNode node = null;
//        try {
//            node = scene.getRootNode();
//        } catch (GrmException ex) {
//            return;
//        }
//        Widget widget = graph.getScene().findWidget(node);
//        widget.getLayout().layout(widget);
//        Point origin = widget.getLocation();
//
//        if ((origin.getX() == 0) || (origin.getY() == 0)) {
//            scene.getLayout().layout(widget);
//            origin = new Point((int) scene.getClientArea().getWidth() / 2, (int) scene.getClientArea().getHeight() / 2);
//            setResolvedNodeLocation(graph, node, origin);
//        }
//
//
//        ResourceProviderNode rpNode = null;
//        try {
//            rpNode = scene.getResourceProviderNode();
//        } catch (GrmException ex) {
//            return;
//        }
//        Collection<ServiceNode> serviceNodez = scene.getServiceNodes();
//
//        /* allocate RP node - make it general way, to show the logic*/
//        double i = 0;
//        if (rpNode != null) {
//            Point rpLoc = new Point(
//                    (int) (Math.cos(i / (serviceNodez.size() + 1) * 2 * Math.PI) * DEFAULT_SVC_RADIUS + origin.getX()),
//                    (int) (Math.sin(i / (serviceNodez.size() + 1) * 2 * Math.PI) * DEFAULT_SVC_RADIUS + origin.getY()));
//            setResolvedNodeLocation(graph, rpNode, rpLoc);
//            Collection<ResourceNode> resourceNodez = scene.getResourceNodes(rpNode);
//            double r = 0;
//            for (ResourceNode rn : resourceNodez) {
//                Point resLoc = new Point(
//                        (int) (Math.cos(r / resourceNodez.size() * 2 * Math.PI) * DEFAULT_RES_RADIUS + rpLoc.getX()),
//                        (int) (Math.sin(r / resourceNodez.size() * 2 * Math.PI) * DEFAULT_RES_RADIUS + rpLoc.getY()));
//                setResolvedNodeLocation(graph, rn, resLoc);
//                r++;
//            }
//        }
//
//        /* allocate service nodez */
//        i = 1;
//        for (ServiceNode n : serviceNodez) {
//            Collection<ResourceNode> resourceNodez = scene.getResourceNodes(n);
//            int count = resourceNodez.size();
//            int max_circles = (int) Math.ceil((-1 + Math.sqrt(1 + 1.6 * count)) / 2);
//            double real_svc_radius = max_circles * 90 + DEFAULT_SVC_RADIUS;
//            Point svcLoc = new Point(
//                    (int) (Math.cos(i / (serviceNodez.size() + 1) * 2 * Math.PI) * real_svc_radius + origin.getX()),
//                    (int) (Math.sin(i / (serviceNodez.size() + 1) * 2 * Math.PI) * real_svc_radius + origin.getY()));
//            setResolvedNodeLocation(graph, n, svcLoc);
//            double r = 1;
//            for (ResourceNode rn : resourceNodez) {
//                /* get current circle */
//                int cur_circle = (int) Math.ceil((-1 + Math.sqrt(1 + 1.6 * r)) / 2);
//                /* transform current circle radius */
//                double real_res_radius = cur_circle * 80 + DEFAULT_RES_RADIUS;
//                /* transform current location */
//                double real_res_location = 0.2 * r / cur_circle - (cur_circle - 1) / 2;
//                Point resLoc = new Point(
//                        (int) (Math.cos(real_res_location * 2 * Math.PI) * real_res_radius + svcLoc.getX()),
//                        (int) (Math.sin(real_res_location * 2 * Math.PI) * real_res_radius + svcLoc.getY()));
//                setResolvedNodeLocation(graph, rn, resLoc);
//                r++;
//            }
//            i++;
//        }
//
//    }
//
//    protected void performNodesLayout(UniversalGraph<Node, Node> universalGraph, Collection<Node> nodes) {
//        // TODO
//        Logger.getLogger(getClass().getName()).log(Level.WARNING, "performNodesLayout is not yet supported");
//    }
//}
//
