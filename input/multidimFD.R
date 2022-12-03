###########################################################################################################################################
#
# 'multidimFD': function to compute and illustrate multidimensional functional diversity indices for a set of species assemblages
# For details about indices formulas see Mouillot et al. 2013, Trends in ecology and Evolution (28:167-177) and references therein.
#                 
# INPUTS: 
#       - 'coord': a matrix with coordinates of species (rows) in a multidimensional functional space (columns)
#		    - 'weight': a matrix with weight of species (columns) in a set of assembalges (rows). 
#                   Weight could be any continuous variable:  biomass, relative biomass, number of individuals, percent coverage.
#                   All FD indices are computed using relative weight so that they are not affected by unit (e.g. g or kg for biomass).
#                   Thus, in the special case where 'weight' is filled with only 0/1 (to code for presence/absence), 
#                           then FD indices will be computed assuming all species have the same weight
#       - 'check_species_pool" : a logical value indicating wether two tests are performed to check that all rows and all columns 
#                               of 'weight' have a sum stricly positive (i.e. all species should be present in at least one assemblage 
#                               and all assemblages should have at least one species). Default is TRUE.
#		    - 'verb': a logical value (default=TRUE) indicating whether printing progress of computation process
#
#     NB :  
#           Column names of 'weight' should be the same than row names of 'coord' (i.e. same codes and same order).
#           If 'coord' has no names, default names will be set ('Axis_1', 'Axis_2',...,'Axis_n').
#           'weight' should have row names
#           NA are not allowed in 'coord' or in 'weight'.
#           Only positive numbers are allowed in 'weight'.
#           There should be at least 2 functional axes and 2 species
#           If some assemblages have fewer species than number of axes, FRic and FDiv indices could not be computed (see below).
#
#       - 'folder_plot': a character string for setting the working directory where plots will be saved, default is current working directory.
# 		  - 'nm_asb_plot': a vector with the names of assemblages for which FD indices need to be illustrated. 
#                       Default is NULL (i.e. no plot). To plot FD for all assemblages, set to 'row.names(weight)'.
#       - 'Faxes_plot': a vector with names of axes to plot. Should be of length from 2 to 4. 
#               Default is NULL which means that the four first axes will be plotted.
#               Value of 'Faxes_plot' does not affect the way FD indices are computed (i.e. acccording to all columns of 'coord').
#       - 'Faxes_nm_plot': a vector with titles of axes (default is titles as 'Faxes_plot') 
# 		  - 'plot_pool': a logical value indicating whether all species present in 'coord' need to be illustrated on all plots.
#                      If yes (default value), space filled by the pool of species is in white while background is filled 
#                         with color specified in 'col_bg' (default is light grey), and absent species are plotted with a different symbol (see below).
#       - 'col_bg': a R color name (or hexadecimal code). See above.
#       - "pch_sp_pool", "cex_sp_pool", 'col_sp_pool': Shape, size and color of symbol to plot absent species. See above.
#       - 'pch_sp': a numeric value coding shape of symbol to plot species, as in function 'points' (default is 21, i.e. points).
#       - 'col_sp': a character string for hexadecimal code (e.g. from www.colorpicker.com) for color used for symbols and convex hull.
# 		  - 'transp': a single numeric value indicating the percentage of transparency for convex hull filling. Default is 50%.
#
#
# OUTPUTS: 
#     => a matrix with for each assemblage (row) values for a set of diversity indices (columns):
#       - 'Nb_sp': number of species present
#       - 'Tot_weight': total weight (e.g. biomass, number of individuals)
#       - 'min_f', 'max_f', 'range_f': minimum, maximum and range of values along 'f' functional axis
#       - 'FIde_f': weighted mean position along 'f' functional axis
#       - 'FRic': functional richness (proportion of functional space filled by species present in the assemblage)
#       - 'FDiv': functional divergence (deviation of species weight to the center of the convex hull filled by the species assemblage)
#       - 'FEve': functional evenness (regularity of distribution of species weights in the functional space)
#       - 'FDis': functional dispersion (weighted mean distance to the average position of the species present in the assemblage 
#                                             dividied by half the maximum distance among all the species present in the set of assemblages )
#       - 'FSpe': functional specialization (weighted mean distance to species pool centroid, i.e. average position of all the species 
#                                           present in the set of assemblages, divided by the maximum distance to this centroid)
#       - 'FOri': functional originality (weighted mean distance to nearest species from the species pool
#                                             divided by the maximum distance to the nearest neighbour)
#
#       NB: FRic and FDiv are computed only for assemblages with more species than number of functional axes
#           FEve is computed only for assemblages of at least 3 species
#           Scaling of FDis, FSpe and FOri indices were scaled by maximum value possible given all the species present in the set of assemblages 
#                  so that they range from 0 to 1, they are unitless and easily interpretable (as FRic, FDiv and FEve)
#
#     => for each of the assemblages listed in 'nm_asb_plot':a 6-panels jpeg file named 'AssemblageA_AxisX_AxisY.jpeg' 
#             illustrating FD indices of assemblage 'A' in the functional space defined by axes 'X' And 'Y. 
#             Jpeg file has a resolution of 150dpi and dimensions of 1800x1200 pixels which means a size of around 200ko 
#
#       All plots (i.e. all assemblages and all pairs of axes) have the same axis scale to faithfully represent FD.
#       Species present in the assemblage are plotted with a 'pch_sp' symbol. 
#       Weights of species are illustrated proportionally to symbol area and a legend is displayed in the bottom right corner.
#
#          * top left panel: functional identity and functional dispersion. 
#                 Weighted-mean position of the species on each axis is illustrated with a square and horizontal and vertical dashed lines. 
#                 Distance to this point are shown with segments
# 			   * top middle panel: functional richness.
#					        The colored convex polygon is a projection of the multidimensional convex hull in 2D. 
#                 Filled symbols are species being vertices in the multidimensional space. 
#                 Minimum and maximum values on each axis are illustrated by vertical bars.
# 			   * top right panel: functional divergence.  
#                 The center of gravity of the vertices is show with a diamond and al the distances to it are shown with lines.
# 			   * bottom left panel: functional evenness. 
#                 The minimum spanning tree linking all points in the multidimensional space is shown.
# 			   * bottom middle panel: functional specialization
#                 Center of gravity of all points is shown as a black square and the distances to it for species present are shown with dotted lines.
# 			   * bottom right panel: functional originalty.
#                 Distances to nearest species in the multidimensional functional space are shown with black arrows (basis=focal species, head=nearest neighbour).
#
##########################################################################################################################################


multidimFD<-function(coord, weight, check_species_pool=TRUE, verb=TRUE,
                      folder_plot=NULL, nm_asb_plot=NULL, Faxes_plot=NULL, Faxes_nm_plot=NULL, 
                      plot_pool=TRUE, col_bg="grey90", col_sp_pool="grey30", pch_sp_pool="+", cex_sp_pool=1,
                      pch_sp=21, col_sp="#1145F0", transp=50 ) 
  
  {
  
  # library required for indices computation
  require (geometry)
  require(ape)
  
  # saving name of current working directory
  current_wd<-getwd()

  ##############################################################################
  # checking inputs


  # coordinates of species in the functional space
  if( nrow(coord)<2 ) stop(paste(" error: there must be at least 2 species in the dataset"))
  if( ncol(coord)<2 ) stop(paste(" error: there must be at least 2 functional axes"))
  if( is.numeric(coord)==FALSE ) stop(paste(" error: 'coord' is not numeric"))
  if( is.na(sum(coord)) ) stop(paste(" error: NA in 'coord'"))
  if ( is.null(colnames(coord)) ) { colnames(coord)<-paste("Axis", 1:ncol(coord),sep="_") } # if no column names in 'coord' default value
  
  # dominance of species in assemblages
  if( is.matrix(weight)==FALSE ) stop( " 'weight' should be an object of type matrix")
  if( is.numeric(weight)==FALSE ) stop(paste(" error: 'weight' is not numeric"))
  if( is.na(sum(weight)) ) stop(paste(" error: NA in 'weight'"))
  if( min(weight)<0 ) stop(paste(" error: negative value in 'weight'"))
  if(min(apply(weight,1,sum))==0 ) 
    stop(paste(" error: all rows of 'weight' should have a sum stricly positive, i.e. all assemblage must host at least one species"))
  
  # match between datasets
  if( sum(colnames(weight) == row.names(coord))!= nrow(coord) ) stop(paste(" error: 'weight' does not have the same column names than row names of 'coord'"))
  
  # checking graphical parameters
  if( length(pch_sp)!=1 ) stop(paste(" error:'pch_sp' should contain only one value"))
  if( length(col_sp)!=1 ) stop(paste(" error:'col_sp' should contain only one value"))
  if( length(col_bg)!=1 ) stop(paste(" error:'col_bg' should contain only one value"))
  if( length(col_sp_pool)!=1 ) stop(paste(" error:'col_sp_pool' should contain only one value"))
  if( length(pch_sp_pool)!=1 ) stop(paste(" error:'pch_sp_pool' should contain only one value"))
  if( length(cex_sp_pool)!=1 ) stop(paste(" error:'cex_sp_pool' should contain only one value"))
  
  
  # checking species pool
  if (check_species_pool==TRUE)
  {
    if(min(apply(weight,2,sum))==0 ) 
      warning(paste("All columns of 'weight' should have a sum stricly positive, i.e. all species must occur in at least one assemblage"))
  }# end of check species pool
  
  
  ##############################################################################
  # info on study case
  
  # number and names of axes
  nm_axes<-colnames(coord)
  nb_axes<-length(nm_axes)
  
  # number and names of assemblages
  nm_asb<-row.names(weight)
  nb_asb<-length(nm_asb)
  
  # matrix to store results
  indices<-c( "Nb_sp", "Tot_weight", paste("min",nm_axes,sep="_"), paste("max",nm_axes,sep="_"), paste("range",nm_axes,sep="_"), 
    paste("FIde",nm_axes,sep="_"), c("FRic","FDiv","FEve","FDis","FSpe", "FOri") )
  FD<-matrix(NA, nb_asb, length(indices), dimnames=list(nm_asb,indices))
  
  ##############################################################################
  # preliminary computation at the species pool level
  
  #######################################
  # originality of each species: distance to nearest neighbour among the global pool of species
  dist_sp<-as.matrix(dist(coord,method="euclidean")) ; dist_sp[which(dist_sp==0)]<-NA
  orig_sp<-apply(dist_sp, 1, min, na.rm=T )
  # identity of Nearest Neighbour
  NN<-dist_sp ; NN<-NN-apply(NN,1,min,na.rm=T) ; NN[which(NN!=0)]<-NA   ; NN[which(NN==0)]<-1
  
  # specialization of each species: distance to centroid of the global pool of species
  centroid_sp<-apply(coord,2,mean) # coordinates of the center of gravity of the vertices (B)
  spec_sp<-apply(coord, 1, function(x) { (sum((x-centroid_sp)^2) )^0.5} )
  
  # convex hull volume of the species pool
  FRic_pool<-convhulln(coord,"FA")$vol
  
  #######################################
  # setting same graphical parameters for all assemblages
  
    # setting working directory to store jpeg files
    if (is.null(folder_plot) ) {  folder_plot<-current_wd }
    
    # setting folder for saving jpeg files
    test_folder_plot<-try( setwd(folder_plot) , silent=TRUE)
    if ( class(test_folder_plot) =="try-error") {
      folder_plot<-current_wd 
      print(paste(" /!\    WARNING: '",folder_plot," does not exist', jpeg files have been saved in '",current_wd, "'",sep="" ))
      } # end of if
    setwd(folder_plot)
  
    # range of 'coord' extended by 5%
    rge_coord<-range(coord)
    extrge_coord<-c( rge_coord[1]-0.05*(rge_coord[2]-rge_coord[1]) , rge_coord[2]+0.05*(rge_coord[2]-rge_coord[1]))
    
    # pretty axes labels within observed range
    lab<-pretty(extrge_coord, n=4) # labels
    lab<-lab[which(lab>=rge_coord[1] & lab<=rge_coord[2]) ]# filtering
    
    # axes limits a,d range
    Faxes_lim<-extrge_coord
    rge_Faxes_lim<-Faxes_lim[2]-Faxes_lim[1]
    
    ###########################################
    # function to draw functional space given axes limits and background color, option: lengend for species weights
    functional_space<-function(axes_xy, nm_axes_xy, col_bg, plot_pool=plot_pool, legend_weight=FALSE) 
      {
      # setting margins size of plot
      par( pty="s", mar=c(3,3,3,3) ) 
      
      plot(Faxes_lim,Faxes_lim,type="n",axes=F,xaxt="n",yaxt="n",xlab="",ylab="",xlim=Faxes_lim,ylim=Faxes_lim) # default window
          rect(Faxes_lim[1],Faxes_lim[1],Faxes_lim[2],Faxes_lim[2], col="white")   # customized window
          
          # customized X and Y axes
          for (k in 1:2)
          {
            axis(side=k, at=lab, labels=F, tcl=-0.3, pos=Faxes_lim[1])  # ticks
            mtext(side=k, lab, at=lab, line=-0.2, cex=0.9, las=1) # labels
            mtext(side=k, nm_axes_xy[k], cex=1,line=1.5, font=2) # title  
          } # end of k
          
          
          if (plot_pool==TRUE)
          {
            # grey background
            rect(Faxes_lim[1],Faxes_lim[1],Faxes_lim[2],Faxes_lim[2], col=col_bg)   
            
            # convex hull of species pool
            vert0<-convhulln( coord[,axes_xy] ,"Fx TO 'vert.txt'")
            vert1<-scan("vert.txt",quiet=T)
            vert_ij<-(vert1+1)[-1]
            polygon(coord[vert_ij,axes_xy], border=NA,col="white")   
            
            # all species
            points( coord[,axes_xy[1] ], coord[,axes_xy[2] ] , pch=pch_sp_pool, col=col_sp_pool, cex=cex_sp_pool)

          }# end of if plot_pool
          
      
          # legend for abundance 
          if (legend_weight==TRUE)  
          {
            rect(max(Faxes_lim)-0.25*rge_Faxes_lim, min(Faxes_lim), max(Faxes_lim), min(Faxes_lim)+0.12*rge_Faxes_lim, col="white")
            symbols(max(Faxes_lim)-0.19*rge_Faxes_lim, min(Faxes_lim)+0.06*rge_Faxes_lim, circles=sqrt(0.1)*0.075*rge_Faxes_lim, 
                                  inches=FALSE, bg="black", fg="black", add=TRUE, lwd=1.5)
            text(max(Faxes_lim)-0.15*rge_Faxes_lim, min(Faxes_lim)+0.06*rge_Faxes_lim,"10%", adj=c(0,0.5) ) 
          }# end of if legend
          
      }# end of function functional_space
    ###########################################
  
  # end of preliminary computation
  
  ##############################################################################
    
  # loop on assemblages for computing and plotting functional diversity indices
  for (k in nm_asb)
  {

    ###########################################################
    # preparing data
    
    # names, number, weight and coordinates of of species present
    weight_k<-weight[k,]
    nm_sp_k<-row.names(coord)[which(weight_k>0)]
    nb_sp_k<-length(nm_sp_k)
    weight_sp_k<-weight[k,nm_sp_k]
    coord_sp_k<-coord[nm_sp_k,]
    if(nb_sp_k==1) { coord_sp_k<-matrix(coord_sp_k,nrow=1,dimnames=list(nm_sp_k,colnames(coord)) ) } # matrix object
    
    # names of species absent
    nm_sp_absent_k<-names(which(weight[k,]==0))
    
    # total weight
    FD[k,"Tot_weight"]<-sum(weight_sp_k)
    
    #relative weight 
    rel_weight_sp_k<-weight_sp_k/sum(weight_sp_k)
    
    # species richness
    FD[k,"Nb_sp"]<-nb_sp_k
    
    ###########################################################
    # computing indices values on each axis
    
    # range of values
    for (z in nm_axes) 
    {
      FD[k,paste("min",z,sep="_")]<-min(coord_sp_k[,z])
      FD[k,paste("max",z,sep="_")]<-max(coord_sp_k[,z])
      FD[k,paste("range",z,sep="_")]<-FD[k,paste("max",z,sep="_")]-FD[k,paste("min",z,sep="_")]
    }# end of z
    
    # abundance-weighted mean values
    FD[k,paste("FIde",nm_axes,sep="_")]<-rel_weight_sp_k%*%coord_sp_k
    
    ###########################################################  
    # multivariate indices
    
    
    # indices based on vertices and volume of convex hull, only if more species than number of axes
 
        if (nb_sp_k>nb_axes) {
          
          ########################
          # Functional richness = convex hull volume
          FD[k,"FRic"]<-round(convhulln(coord_sp_k,"FA")$vol/FRic_pool,6)
          

          ########################
          # Functional Divergence
          
          # identity of vertices
          vert0<-convhulln(coord_sp_k,"Fx TO 'vert.txt'")
          vert1<-scan("vert.txt",quiet=T)
          vertices_k<-(vert1+1)[-1]
          
          # coordinates of the center of gravity of the vertices (B)
          B_k<-apply(coord_sp_k[vertices_k,],2,mean)
          
          # Euclidean dstance to B (dB)
          dB_k<-apply(coord_sp_k, 1, function(x) { (sum((x-B_k)^2) )^0.5} )
          
          # mean of dB values and deviations to mean 
          meandB_k<-mean(dB_k)
          devdB_k<-dB_k-meandB_k
          
          # abundance-weighted mean deviation
          abdev_k<- rel_weight_sp_k*devdB_k
          ababsdev_k<- rel_weight_sp_k*abs(devdB_k)
          
          FD[k,"FDiv"]<-round( (sum(abdev_k)+meandB_k) / (sum(ababsdev_k)+meandB_k) ,6)
          
          
        }# end of if more species than number of axes
    
    ##########################
    # Functional Evenness

    if (nb_sp_k>=3) {
      
    # inter-species Euclidean distance
    distT_k<-dist(coord_sp_k, method="euclidian")
    
    # topology of Minimum Spanning Tree and conversion of the 'mst' matrix into 'dist' class
    linkmst_k<-mst(distT_k)
    mstvect_k<-as.dist(linkmst_k)
    
    # pairwise cumulative relative abundances and conversion into 'dist' class
    ab2_k<-matrix(0,nrow=nb_sp_k,ncol=nb_sp_k)
    for (q in 1:nb_sp_k)
      for (r in 1:nb_sp_k)
        ab2_k[q,r]<-rel_weight_sp_k[q]+rel_weight_sp_k[r] # end of q,r
    ab2vect_k<-as.dist(ab2_k)
    
    # EW index for the (S-1) segments
    EW_k<-rep(0,nb_sp_k-1)
    flag<-1
    for (m in 1:((nb_sp_k-1)*nb_sp_k/2))
    {if (mstvect_k[m]!=0) {EW_k[flag]<-distT_k[m]/(ab2vect_k[m]) ; flag<-flag+1}}  # end of m
    
    # PEW index and comparison with 1/S-1
    minPEW_k<-rep(0,nb_sp_k-1)  ;  OdSmO_k<-1/(nb_sp_k-1)
    for (l in 1:(nb_sp_k-1))
      minPEW_k[l]<-min( (EW_k[l]/sum(EW_k)) , OdSmO_k )  # end of l
    
    # FEve
    FD[k,"FEve"]<-round( ( (sum(minPEW_k))- OdSmO_k) / (1-OdSmO_k ) ,6)
   
     }# end of if at least 3 species
    
    ##########################
    # Functional Dispersion: abundance-weighted mean distance to abundance-weighted centroid 
    # scaled by maximum value possible given species pool (i.e. the two most distant species have half of total weight)
    dist_centr_sp_k<-apply(coord_sp_k, 1, function(x) { (sum((x-FD[k,paste("FIde",nm_axes,sep="_")])^2) )^0.5} ) # distance to abundance-weighted centroid 
    FD[k,"FDis"]<-(rel_weight_sp_k %*% dist_centr_sp_k) / ( max(dist_sp, na.rm=T) /2 )
    
    ##########################
    # functional specialization : abundance-weighted mean distance to centroid of the global pool of species
    # scaled by maximum value possible given species pool (i.e. an assmeblage hosting only the most specialized species)
    FD[k,"FSpe"]<-(rel_weight_sp_k %*% spec_sp[nm_sp_k])/ max(spec_sp)
    
    ##########################
    # functional originality : abundance-weighted mean distance to nearest neighbour in the global pool of species 
    # scaled by maximum value possible given species pool (i.e. an assmeblage hosting only the most original species)
    FD[k,"FOri"]<-(rel_weight_sp_k %*% orig_sp[nm_sp_k])/max(orig_sp)
    
    
    ######################################################################################################################
    # End of indices computation
    ######################################################################################################################
    
    
    ###########################################################
    # if graphical output
    
    if (k %in% nm_asb_plot)
    {
  
      # axes to plot if not specified: up to first 4 axes 
      if ( is.null(Faxes_plot) ) { Faxes_plot<-colnames(coord)[ 1:(min(c(ncol(coord),4)))]  }
      
      # igf not specififed default axes names
      if ( is.null(Faxes_nm_plot) ) { Faxes_nm_plot<-Faxes_plot }
      
      # checking inputs
      if( sum( nm_asb_plot %in% row.names(weight) ) != length(nm_asb_plot) ) stop(paste(" error: 'nm_asb_plot' should be subset of 'coord' row names"))
      
      if( length(Faxes_plot) <2 | length(Faxes_plot) >4) stop(paste("length of 'Faxes_plot' should be 2, 3 or 4 "))
      if( sum( Faxes_plot %in% colnames(coord) ) != length(Faxes_plot) ) stop(paste(" error: 'Faxes_plot' should be subset of 'coord' column names"))
      if( length(Faxes_plot) != length(Faxes_nm_plot) ) stop(paste("length of 'Faxes_plot' should match length of 'Faxes_nm_plot' "))
      
      # shortening object names
      Faxes<-Faxes_plot
      Faxes_nm<-Faxes_nm_plot
      
      # number of axes
      nb_Faxes<-length(Faxes_plot)
      
      ################################
      # loop on pairs of axes
      for (i in 1:(nb_Faxes-1) )
        for (j in (i+1):nb_Faxes )
        {
          
          # creating jpeg file with 8 panels
          nmjpeg<-paste(k,"_",Faxes[i] ,"_",Faxes[j],".jpeg",sep="")
          jpeg(file=nmjpeg, res=150, width=1800, height=1200)
          layout(matrix(c(1:6),2,3,T)) ; layout.show(6)
          
          # setting margins size of plot
          par( pty="s", mar=c(3,3,3,3) ) 
          
          ################################
          # First panel = Functional Identity and Functional dispersion
          functional_space( c(Faxes[i] ,Faxes[j]), c(Faxes_nm[i] ,Faxes_nm[j]), col_bg=col_bg, plot_pool=plot_pool, legend_weight=TRUE )

          # mean position on each axis 
          segments(FD[k,paste("FIde_",Faxes[i],sep="")],FD[k,paste("FIde_",Faxes[j],sep="")], FD[k,paste("FIde_",Faxes[i],sep="")], min(Faxes_lim), lwd=1.5, col=col_sp, lty=2)
          segments(FD[k,paste("FIde_",Faxes[i],sep="")],FD[k,paste("FIde_",Faxes[j],sep="")], min(Faxes_lim) ,FD[k,paste("FIde_",Faxes[j],sep="")], lwd=1.5, col=col_sp, lty=2)
          points( FD[k,paste("FIde_",Faxes[i],sep="")],FD[k,paste("FIde_",Faxes[j],sep="")], pch=22, bg=col_sp, col=col_sp,cex=2.5)
          
          # distance to abundance weighted centroid
          segments( FD[k,paste("FIde_",Faxes[i],sep="")],FD[k,paste("FIde_",Faxes[j],sep="")], coord_sp_k[,Faxes[i]], coord_sp_k[,Faxes[j]],col=col_sp, lty=1, lwd=2)
          
          # abundances, scaling: point area proportional to relative abundance, if relab=100%, circle diameter=15% of axis range
          sizeab_k<-sqrt(rel_weight_sp_k)*0.075*rge_Faxes_lim
          o_weight<-order(rel_weight_sp_k, decreasing = TRUE)
          symbols(coord_sp_k[o_weight,Faxes[i]], coord_sp_k[o_weight,Faxes[j]], circles=sizeab_k[o_weight], inches=FALSE, 
            bg=col_sp, fg="black", add=TRUE)
          
          # FDis and FIde values
          mtext(side=3, paste("FDis=",round(FD[k,'FDis'],3),sep=""), at=mean(Faxes_lim), line=0.8, cex=1.1,adj=0.5, font=2)               
          mtext(side=3, paste("FIde(x)=",round(FD[k,paste("FIde_",Faxes[i],sep="")],3),sep=""), at=min(Faxes_lim), line=-0.4, cex=0.9,adj=0, font=2)     
          mtext(side=3, paste("FIde(y)=",round(FD[k,paste("FIde_",Faxes[j],sep="")],3),sep=""), at=max(Faxes_lim), line=-0.4, cex=0.9,adj=1, font=2)     
          
          ################################
          # Second panel = functional richness
          functional_space( c(Faxes[i] ,Faxes[j]), c(Faxes_nm[i] ,Faxes_nm[j]), col_bg=col_bg, plot_pool=plot_pool, legend_weight=TRUE )
          
          # range on each axis
          dec1<-rge_Faxes_lim *0.02
          segments( FD[k,paste("min_",Faxes[i],sep="")], min(Faxes_lim)-dec1, FD[k,paste("min_",Faxes[i],sep="")], min(Faxes_lim)+dec1, col=col_sp , lwd=3) # min x
          segments( FD[k,paste("max_",Faxes[i],sep="")], min(Faxes_lim)-dec1, FD[k,paste("max_",Faxes[i],sep="")], min(Faxes_lim)+dec1, col=col_sp , lwd=3) # max x
          segments( min(Faxes_lim)-dec1, FD[k,paste("min_",Faxes[j],sep="")], min(Faxes_lim)+dec1, FD[k,paste("min_",Faxes[j],sep="")],  col=col_sp , lwd=3) # min y
          segments( min(Faxes_lim)-dec1, FD[k,paste("max_",Faxes[j],sep="")], min(Faxes_lim)+dec1, FD[k,paste("max_",Faxes[j],sep="")],  col=col_sp , lwd=3) # max y
          
          
          # projected convex hull in 2D
          if (nb_sp_k>nb_axes) {
            
          vert0<-convhulln(coord_sp_k[,Faxes[c(i,j)]],"Fx TO 'vert.txt'")
          vert1<-scan("vert.txt",quiet=T) ; vertices2D<-(vert1+1)[-1]
          polygon(coord_sp_k[vertices2D,Faxes[c(i,j)]],border=NA,col=paste(col_sp,transp,sep=""))
          
          # all points (empty) then filling points being vertices in nD
          points(coord_sp_k[,Faxes[i]], coord_sp_k[,Faxes[j]], pch=21, bg="white", col="black",cex=2)
          points(coord_sp_k[vertices_k,Faxes[i]], coord_sp_k[vertices_k,Faxes[j]], pch=21,bg=col_sp, col="black",cex=2)
          
          # index    
          mtext(side=3, paste("FRic=",round(FD[k,'FRic'],3),sep=""), at=mean(Faxes_lim), line=0.8, cex=1.1,adj=0.5, font=2)  
          }# end of FRic computable
          
          mtext(side=3, paste(Faxes_nm[i], " [",round(FD[k,paste("min_",Faxes[i],sep="")],1),";",round(FD[k,paste("max_",Faxes[i],sep="")],1),"]",sep=""),
            at=min(Faxes_lim), line=-0.4, cex=0.8,adj=0) 
          mtext(side=3, paste(Faxes_nm[j], " [",round(FD[k,paste("min_",Faxes[j],sep="")],1),";",round(FD[k,paste("max_",Faxes[j],sep="")],1),"]",sep=""),
            at=max(Faxes_lim), line=-0.4, cex=0.8,adj=1) 
          
          ###############################################################
          # Third panel = functional Divergence
          functional_space( c(Faxes[i] ,Faxes[j]), c(Faxes_nm[i] ,Faxes_nm[j]), col_bg=col_bg, plot_pool=plot_pool, legend_weight=TRUE )
          
            if (nb_sp_k>nb_axes) {
            # projected convex hull in 2D
            vert0<-convhulln(coord_sp_k[,Faxes[c(i,j)]],"Fx TO 'vert.txt'")
            vert1<-scan("vert.txt",quiet=T) ; vertices2D<-(vert1+1)[-1]
            polygon(coord_sp_k[vertices2D,Faxes[c(i,j)]],border=NA,col=paste(col_sp,transp,sep=""))
            
            # distance to centroid of vertices
            segments( B_k[Faxes[i]], B_k[Faxes[j]], coord_sp_k[,Faxes[i]], coord_sp_k[,Faxes[j]],col=col_sp, lty=1, lwd=2)
            points( B_k[Faxes[i]], B_k[Faxes[j]], pch=23,col=col_sp, bg=col_sp,cex=2.5)
            }# end of FRic computable
            
            # abundances, scaling: point area proportional to relative abundance, if relab=100%, circle diameter=15% of axis range
          sizeab_k<-sqrt(rel_weight_sp_k)*0.075*rge_Faxes_lim
          o_weight<-order(rel_weight_sp_k, decreasing = TRUE)
          symbols(coord_sp_k[o_weight,Faxes[i]], coord_sp_k[o_weight,Faxes[j]], circles=sizeab_k[o_weight], inches=FALSE, 
            bg=col_sp, fg="black", add=TRUE)
          
            # FDiv index    
            mtext(side=3, paste("FDiv=",round(FD[k,'FDiv'],3),sep=""), at=mean(Faxes_lim), line=-0.1, cex=1.1,adj=0.5, font=2)               
            
            ###############################################################
            # Fourth panel = functional evenness
            functional_space( c(Faxes[i] ,Faxes[j]), c(Faxes_nm[i] ,Faxes_nm[j]), col_bg=col_bg, plot_pool=plot_pool, legend_weight=TRUE )
            
            # MST
            if (nb_sp_k>=3) {
              for (x in 1:nrow(linkmst_k))
              for (y in 1:nrow(linkmst_k))
                if (linkmst_k[y,x]==1 & y>x) {
                  segments(coord_sp_k[y,Faxes[i]], coord_sp_k[y,Faxes[j]],coord_sp_k[x,Faxes[i]], coord_sp_k[x,Faxes[j]], 
                  col=col_sp, lwd=1.5) }# end of if link on MST
            }# end of FEve computable
            
            # abundances, scaling: point area proportional to relative abundance, if relab=100%, circle diameter=15% of axis range
            sizeab_k<-sqrt(rel_weight_sp_k)*0.075*rge_Faxes_lim
            o_weight<-order(rel_weight_sp_k, decreasing = TRUE)
            symbols(coord_sp_k[o_weight,Faxes[i]], coord_sp_k[o_weight,Faxes[j]], circles=sizeab_k[o_weight], inches=FALSE, 
              bg=col_sp, fg="black", add=TRUE)
            
            # FEve index    
            mtext(side=3, paste("FEve=",round(FD[k,'FEve'],3),sep=""), at=mean(Faxes_lim), line=-0.1, cex=1.1,adj=0.5, font=2)               
            
          
            ###############################################################
            # Fifth panel = functional specialization
            functional_space( c(Faxes[i] ,Faxes[j]), c(Faxes_nm[i] ,Faxes_nm[j]), col_bg=col_bg, plot_pool=plot_pool, legend_weight=TRUE )
            
            # distance to centroid of all points
            segments( centroid_sp[Faxes[i]], centroid_sp[Faxes[j]], coord_sp_k[,Faxes[i]], coord_sp_k[,Faxes[j]],col=col_sp, lty=3, lwd=2)
            points( centroid_sp[Faxes[i]], centroid_sp[Faxes[j]], pch=23,col="black",bg="black",cex=2.5)
            
            # abundances, scaling: point area proportional to relative abundance, if relab=100%, circle diameter=15% of axis range
            sizeab_k<-sqrt(rel_weight_sp_k)*0.075*rge_Faxes_lim
            o_weight<-order(rel_weight_sp_k, decreasing = TRUE)
            symbols(coord_sp_k[o_weight,Faxes[i]], coord_sp_k[o_weight,Faxes[j]], circles=sizeab_k[o_weight], inches=FALSE, 
              bg=col_sp, fg="black", add=TRUE)
            
            # FSpe index    
            mtext(side=3, paste("FSpe=",round(FD[k,'FSpe'],3),sep=""), at=mean(Faxes_lim), line=-0.1, cex=1.1,adj=0.5, font=2)               
            
            ###############################################################
            # Sixth panel = functional originality
            functional_space( c(Faxes[i] ,Faxes[j]), c(Faxes_nm[i] ,Faxes_nm[j]), col_bg=col_bg, plot_pool=plot_pool, legend_weight=TRUE )
            
            for (z in row.names(coord_sp_k) )
            {
              nm_NN_z<-names(which(NN[z,]==1)[1])
            arrows( coord_sp_k[z,Faxes[i]], coord_sp_k[z,Faxes[j]], coord[nm_NN_z,Faxes[i]], coord[nm_NN_z,Faxes[j]],
                col="black", lwd=1.8, length=0.1, angle=20)
            } # end of k
            
            # abundances, scaling: point area proportional to relative abundance, if relab=100%, circle diameter=15% of axis range
            sizeab_k<-sqrt(rel_weight_sp_k)*0.075*rge_Faxes_lim
            o_weight<-order(rel_weight_sp_k, decreasing = TRUE)
            symbols(coord_sp_k[o_weight,Faxes[i]], coord_sp_k[o_weight,Faxes[j]], circles=sizeab_k[o_weight], inches=FALSE, 
              bg=col_sp, fg="black", add=TRUE)
            
            # FSpe index    
            mtext(side=3, paste("FOri=",round(FD[k,'FOri'],3),sep=""), at=mean(Faxes_lim), line=-0.1, cex=1.1,adj=0.5, font=2)               
      
        }# end of i,j (pair of axes)
      
      # closing jpeg
      graphics.off()
      ################################
          
          
    }# end of plot of FD indices
    ###########################################################
  
    # printing step achieved
    if (verb==TRUE) print(paste("FD of assemblage '",k,"' computed",sep="") )
  }# end of working on assemblage k
  ###########################################################  
  
  # returning to current working directory
  setwd(current_wd)
  
  # returning results	
  return(FD)	

}# end of function multidimFD
########################################################################################################################################
########################################################################################################################################
