//Load the aqwaapp functionalities in line with the scripts in aqwapages
var aqwaAppletLoaded = this.WB.AppletList.Applet("AQWAApplet").App;
var aqwaScript = aqwaAppletLoaded.Script;
var depth = 200;
// set the water depth to 500
//geomStr = aqwaScript.localString("ID_Geometry");
SelectTreeNode(aqwaScript.localString("ID_Geometry"));
SetDetailText(aqwaScript.localString("ID_WaterDepth"), depth);
SetDetailText(aqwaScript.localString("ID_WaterSizeX"), 2000);
SetDetailText(aqwaScript.localString("ID_WaterSizeY"), 2000);
//strID=SelectPopUpMenu(aqwaScript.localString("ID_Add\ID_PointMass"));
//strID=aqwaScript.localString("KAQWAStructureMass")

///*
// add mass
SelectTreeNode("semi");
aqwaScript.AddAQWATreeItem(aqwaScript.localString("KAQWAStructureMass"),aqwaScript.localString("KPointMass"))
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("mass_semi"));
SetDetailText(aqwaScript.localString("ID_StrMassDefinition"), aqwaScript.localString("ID_ManualDefinition"));

SetDetailText(aqwaScript.localString("ID_StrMassX"), 0);
SetDetailText(aqwaScript.localString("ID_StrMassY"), 0);
SetDetailText(aqwaScript.localString("ID_StrMassZ"), -14.94);

SetDetailText(aqwaScript.localString("ID_StrMassValue"), 1000*18200);
SetDetailText(aqwaScript.localString("ID_StrMassKxx"), 25);
SetDetailText(aqwaScript.localString("ID_StrMassKyy"), 25);
SetDetailText(aqwaScript.localString("ID_StrMassKzz"), 114);
//var mass1_id=aqwaScript.AddAQWATreeItem(aqwaScript.localString("KAQWAStructureMass"),aqwaScript.localString("KPointMass"))

SelectTreeNode("semi");
aqwaScript.AddAQWATreeItem(aqwaScript.localString("KAQWAStructureMass"),aqwaScript.localString("KPointMass"))
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("mass_tower"));
SetDetailText(aqwaScript.localString("ID_StrMassDefinition"), aqwaScript.localString("ID_ManualDefinition"));

SetDetailText(aqwaScript.localString("ID_StrMassX"), 0);
SetDetailText(aqwaScript.localString("ID_StrMassY"), 0);
SetDetailText(aqwaScript.localString("ID_StrMassZ"), 56.5);

SetDetailText(aqwaScript.localString("ID_StrMassValue"), 1000*1363);
SetDetailText(aqwaScript.localString("ID_StrMassKxx"), 2.7);
SetDetailText(aqwaScript.localString("ID_StrMassKyy"), 2.7);
SetDetailText(aqwaScript.localString("ID_StrMassKzz"), 2.7);

SelectTreeNode("semi");
aqwaScript.AddAQWATreeItem(aqwaScript.localString("KAQWAStructureMass"),aqwaScript.localString("KPointMass"))
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("mass_RNA"));

SetDetailText(aqwaScript.localString("ID_StrMassDefinition"), aqwaScript.localString("ID_ManualDefinition"));

SetDetailText(aqwaScript.localString("ID_StrMassX"), 0);
SetDetailText(aqwaScript.localString("ID_StrMassY"), 0);
SetDetailText(aqwaScript.localString("ID_StrMassZ"), 150);

SetDetailText(aqwaScript.localString("ID_StrMassValue"), 1000*991);
SetDetailText(aqwaScript.localString("ID_StrMassKxx"), 3.1);
SetDetailText(aqwaScript.localString("ID_StrMassKyy"), 3.1);
SetDetailText(aqwaScript.localString("ID_StrMassKzz"), 3.1);

//hull drag
SelectTreeNode("semi");
aqwaScript.AddAQWATreeItem(aqwaScript.localString("KAQWAMatrix"),aqwaScript.localString("KMorisonHullDragMatrix"))

//attachement points
SelectTreeNode("semi");
aqwaScript.AddAQWATreeItem(aqwaScript.localString("KAQWAAttachPoint"),1)
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("p1_semi"));
SetDetailText(aqwaScript.localString("ID_AttachPtX"), -60*Math.sin(90 * (Math.PI / 180)));
SetDetailText(aqwaScript.localString("ID_AttachPtY"), -60*Math.cos(90 * (Math.PI / 180)));

SelectTreeNode("semi");
aqwaScript.AddAQWATreeItem(aqwaScript.localString("KAQWAAttachPoint"),1)
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("p2_semi"));
SetDetailText(aqwaScript.localString("ID_AttachPtX"), -60*Math.sin(-30 * (Math.PI / 180)));
SetDetailText(aqwaScript.localString("ID_AttachPtY"), -60*Math.cos(-30 * (Math.PI / 180)));

SelectTreeNode("semi");
aqwaScript.AddAQWATreeItem(aqwaScript.localString("KAQWAAttachPoint"),1)
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("p3_semi"));
SetDetailText(aqwaScript.localString("ID_AttachPtX"), -60*Math.sin(210 * (Math.PI / 180)));
SetDetailText(aqwaScript.localString("ID_AttachPtY"), -60*Math.cos(210 * (Math.PI / 180)));

//fixed points
SelectTreeNode("Fixed Points");
aqwaScript.AddAQWATreeItem(aqwaScript.localString("KAQWAAttachPoint"),1)
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("p1_fixed"));
SetDetailText(aqwaScript.localString("ID_AttachPtX"), -840*Math.sin(90 * (Math.PI / 180)));
SetDetailText(aqwaScript.localString("ID_AttachPtY"), -840*Math.cos(90 * (Math.PI / 180)));
SetDetailText(aqwaScript.localString("ID_AttachPtZ"), -1.0*depth);

SelectTreeNode("Fixed Points");
aqwaScript.AddAQWATreeItem(aqwaScript.localString("KAQWAAttachPoint"),1)
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("p2_fixed"));
SetDetailText(aqwaScript.localString("ID_AttachPtX"), -840*Math.sin(-30 * (Math.PI / 180)));
SetDetailText(aqwaScript.localString("ID_AttachPtY"), -840*Math.cos(-30 * (Math.PI / 180)));
SetDetailText(aqwaScript.localString("ID_AttachPtZ"), -1.0*depth);

SelectTreeNode("Fixed Points");
aqwaScript.AddAQWATreeItem(aqwaScript.localString("KAQWAAttachPoint"),1)
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("p3_fixed"));
SetDetailText(aqwaScript.localString("ID_AttachPtX"), -840*Math.sin(210 * (Math.PI / 180)));
SetDetailText(aqwaScript.localString("ID_AttachPtY"), -840*Math.cos(210 * (Math.PI / 180)));
SetDetailText(aqwaScript.localString("ID_AttachPtZ"), -depth);

// Cable section
SelectTreeNode("Connection Data");
aqwaScript.AddAQWATreeItem(aqwaScript.localString("KAQWAComposite"),aqwaScript.localString("KCableStrStrLinear"))
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("chain_1"));
SetDetailText(aqwaScript.localString("ID_CSMass"), 685);
SetDetailText(aqwaScript.localString("ID_CSECSA"), 0.05);
SetDetailText(aqwaScript.localString("ID_CSStiffnessEA"), 3270000000);
SetDetailText(aqwaScript.localString("ID_CSMaxTension"), 22286000);
SetDetailText(aqwaScript.localString("ID_CSEquivDiam"), 0.2);

// cables connection ------------------needs to be generalized
SelectTreeNode("Connections");
aqwaScript.AddAQWATreeItem(aqwaScript.localString("KAQWACable"),aqwaScript.localString("KCableSection"))
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("MooringLine_1"));
SetDetailText(aqwaScript.localString("ID_CType"), aqwaScript.localString("Nonlinear Catenary"));
SetDetailText(aqwaScript.localString("ID_CConnectivity"), aqwaScript.localString("Fixed Point to Structure"));
SetDetailText(aqwaScript.localString("ID_CEndConnectionPoint"),"p1_semi (semi)");
SetDetailText(aqwaScript.localString("ID_CStartFixedPoint"),"p1_fixed (fixed)");
SetDetailText(aqwaScript.localString("ID_CCatSectionType1"),"chain_1");

SetDetailText(aqwaScript.localString("ID_CCatSectionLength1"),850);
SetDetailText(aqwaScript.localString("ID_CEndConnectionNegativeDZ"),1);
SetDetailText(aqwaScript.localString("ID_CEndConnectionPositiveDZ"),1);


SelectTreeNode("Connections");
aqwaScript.AddAQWATreeItem(aqwaScript.localString("KAQWACable"),aqwaScript.localString("KCableSection"))
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("MooringLine_2"));
SetDetailText(aqwaScript.localString("ID_CType"), aqwaScript.localString("Nonlinear Catenary"));
SetDetailText(aqwaScript.localString("ID_CConnectivity"), aqwaScript.localString("Fixed Point to Structure"));
SetDetailText(aqwaScript.localString("ID_CEndConnectionPoint"),"p2_semi (semi)");
SetDetailText(aqwaScript.localString("ID_CStartFixedPoint"),"p2_fixed (fixed)");
SetDetailText(aqwaScript.localString("ID_CCatSectionType1"),"chain_1");

SetDetailText(aqwaScript.localString("ID_CCatSectionLength1"),850);
SetDetailText(aqwaScript.localString("ID_CEndConnectionNegativeDZ"),1);
SetDetailText(aqwaScript.localString("ID_CEndConnectionPositiveDZ"),1);


SelectTreeNode("Connections");
aqwaScript.AddAQWATreeItem(aqwaScript.localString("KAQWACable"),aqwaScript.localString("KCableSection"))
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("MooringLine_3"));
SetDetailText(aqwaScript.localString("ID_CType"), aqwaScript.localString("Nonlinear Catenary"));
SetDetailText(aqwaScript.localString("ID_CConnectivity"), aqwaScript.localString("Fixed Point to Structure"));
SetDetailText(aqwaScript.localString("ID_CEndConnectionPoint"),"p3_semi (semi)");
SetDetailText(aqwaScript.localString("ID_CStartFixedPoint"),"p3_fixed (fixed)");
SetDetailText(aqwaScript.localString("ID_CCatSectionType1"),"chain_1");

SetDetailText(aqwaScript.localString("ID_CCatSectionLength1"),850);
SetDetailText(aqwaScript.localString("ID_CEndConnectionNegativeDZ"),1);
SetDetailText(aqwaScript.localString("ID_CEndConnectionPositiveDZ"),1);


//======================================== Analysis=======================================================
// HD settings
SelectTreeNode(aqwaScript.localString("Analysis Settings"));
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("Analysis Settings HD"));
SetDetailText(aqwaScript.localString("ID_GOON"), aqwaScript.localString("ID_Yes"));

// Static settings (Equillibrium)
SelectTreeNode(aqwaScript.localString("Hydrodynamic Response"));
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("Hydrodynamic Response EQ"));

SelectTreeNode(aqwaScript.localString("Analysis Settings"));
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("Analysis Settings EQ"));
SetDetailText(aqwaScript.localString("ID_ComputationType"), aqwaScript.localString("Stability Analysis"));

//Waves
SelectTreeNode(aqwaScript.localString("Hydrodynamic Response EQ"));
aqwaScript.AddAQWATreeItem(aqwaScript.localString("KAQWAIrregularWave"),aqwaScript.localString("KWaveJonswapHs"))
SetDetailText(aqwaScript.localString("ID_IWDirectionofSpectrum"), 0);
SetDetailText(aqwaScript.localString("ID_IWSignificantWaveHeight"), 2);
SetDetailText(aqwaScript.localString("ID_IWGamma"), 3);
SetDetailText(aqwaScript.localString("ID_IWPeakFrequency"), 0.1);

// Dynamic settings (Time History)
SelectTreeNode(aqwaScript.localString("Hydrodynamic Response 2"));
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("Hydrodynamic Response HR"));

SelectTreeNode(aqwaScript.localString("Analysis Settings"));
SetDetailText(aqwaScript.localString("ID_Name"), aqwaScript.localString("Analysis Settings HR"));

SelectTreeNode(aqwaScript.localString("Analysis Settings HR"));

SetDetailText(aqwaScript.localString("ID_ComputationType"), aqwaScript.localString("Time Response Analysis"));
SetDetailText(aqwaScript.localString("ID_StartTime"), 0.0);
SetDetailText(aqwaScript.localString("ID_TimeStep"), 0.1);
SetDetailText(aqwaScript.localString("ID_Duration"), 100);
SetDetailText(aqwaScript.localString("ID_OutputTimeStep"), 0.1);

//Waves
SelectTreeNode(aqwaScript.localString("Hydrodynamic Response HR"));
aqwaScript.AddAQWATreeItem(aqwaScript.localString("KAQWAIrregularWave"),aqwaScript.localString("KWaveJonswapHs"))
SetDetailText(aqwaScript.localString("ID_IWDirectionofSpectrum"), 0);
SetDetailText(aqwaScript.localString("ID_IWSignificantWaveHeight"), 6);
SetDetailText(aqwaScript.localString("ID_IWGamma"), 3);
SetDetailText(aqwaScript.localString("ID_IWPeakFrequency"), 0.1);

function SelectTreeNode(strNodeName) {
    var tree = aqwaScript.tv;

	// First try to find an exact match
    for (var i = 1; i <= tree.Nodes.Count; i++) {
        if (tree.Nodes(i).Text == strNodeName) {
            tree.SelectedItem = tree.Nodes(i);
            return tree.SelectedItem.Tag.ID;
        }
    }
	
	// If no exact match, try to find a node which starts with the search name
    for (var i = 1; i <= tree.Nodes.Count; i++) {
		if (tree.Nodes(i).Text.substr(0, strNodeName.length) == strNodeName) {
            tree.SelectedItem = tree.Nodes(i);
            return tree.SelectedItem.Tag.ID;
        }
    }
}

function SetDetailText(str, val) {
    var lv = aqwaScript.lv;
	
    ExpandAllDetailBranches(lv);
	
    try
    {
        lv.ActivateItem(str);
    }
    catch (err)
    {
        throw "AQWASetDetailText : detail View item \"" + str + "\" cannot be found"; 
    }
        
    if (lv.SelectedItem.name != str) {
        throw "AQWASetDetailText : detail View item \"" + str + "\" cannot be found";
    }

    lv.ItemValue = "" + val; // Set the value
}

function ExpandAllDetailBranches(lv) {

    try {
        var count = lv.GroupCount;
        for (var i = 1; i <= count; i++)
            lv.FindGroup(i).Expand = true;
    }
    catch (e) {
        throw "An error occured while expanding all detail branches";
    }
}