
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
List
sorcering(
              const Nullable<NumericMatrix&> A = R_NilValue, 
              const int t_sim = 2,
              const String& tsteps ="monthly",              
              const Nullable<NumericVector&> C0 = R_NilValue, 
              const Nullable<NumericVector&> N0 = R_NilValue, 
              const Nullable<NumericMatrix&> Cin = R_NilValue,
              const Nullable<NumericMatrix&> Nin = R_NilValue,
              const Nullable<NumericMatrix&> xi = R_NilValue,
              const bool calcN = false,
              const bool calcNbalance = false
             ) 
{    
    // sorcering V.0.9.2 July 2021
    // SORCERING: Soil ORganic Carbon & CN Ratio drIven Nitrogen modellinG framework
    // by 
    // Dr. Marc Scherstjanoi, Thünen Institute of Forest Ecosystems, Eberswalde, Germany &
    // Dr. René Dechow, Thünen Institute of Climate-Smart Agriculture, Braunschweig, Germany
    
     Environment base = Environment("package:base");
     Function readline = base["readline"];
    
    //////////////////////////////////////////////////////////////////////////////////////////////////////////
    //input transformation
    //////////////////////////////////////////////////////////////////////////////////////////////////////////
        
    //time stuff
    std::string annually = "annually";
    std::string monthly = "monthly";
    std::string weekly = "weekly";

    int timediv = 12;
    if (tsteps.get_cstring()==monthly)timediv=12;
    if (tsteps.get_cstring()==annually)timediv=1;
    if (tsteps.get_cstring()==weekly)timediv=53;

    int nr_pools=2;
    
    // transfer matrix stuff
    NumericMatrix A_(nr_pools,nr_pools);   //if no A 
    if (A.isNotNull()) 
    {   
        NumericMatrix AX(A);
        A_=AX;
    }
    arma::mat A_arma = as<arma::mat>(A_);
    nr_pools = A_arma.n_rows; // A defines nr_pools    
    
    // input, starting conditions, environment
    NumericVector C0_ (nr_pools); 
    NumericVector N0_ = C0_/10;
    if (C0.isNotNull()) C0_=C0;
    if (N0.isNotNull()) N0_=N0;
    
    NumericMatrix Cin_(t_sim,nr_pools); 
    NumericMatrix Nin_ = Cin_/10;
    NumericVector v (t_sim*nr_pools,1.0);
    NumericMatrix xi_(t_sim,nr_pools,v.begin());

    if (Cin.isNotNull()) 
    {
        NumericMatrix CinX(Cin);
        Cin_=CinX;
    }
    if (Nin.isNotNull()) 
    {
        NumericMatrix NinX(Nin);
        Nin_=NinX;
    }
    
    if (xi.isNotNull()) 
    {   
        NumericMatrix xiX(xi);
        xi_=xiX;
    }

    // Rccp to RccpArmadillo
    arma::vec C0_arma = as<arma::vec>(C0_);
    arma::vec N0_arma = as<arma::vec>(N0_);
        
    arma::mat Cin_arma = as<arma::mat>(Cin_);
    arma::mat Nin_arma = as<arma::mat>(Nin_);
    
    arma::mat xi_arma = as<arma::mat>(xi_);
 
    //////////////////////////////////////////////////////////////////////////////////////////////////////////
    //input verification
    //////////////////////////////////////////////////////////////////////////////////////////////////////////
     
    String s1 ="";
    String s2 = nr_pools;
    String s3 ="";
    unsigned int tseqlength_unsi = t_sim;
    int tseqlength = t_sim;
    String s4 =tseqlength;
    unsigned int nr_p_unsi = nr_pools;

    if (A_arma.n_rows != A_arma.n_cols)  s1 +=(" Matrix A must be quadratic! ");
    if (C0_arma.n_elem != nr_p_unsi) s1 +=(" Length of C0 must match number of pools! ");   
    if (N0_arma.n_elem != nr_p_unsi) s1 +=(" Length of N0 dimensions must match number of pools! ");    
    if (Cin_arma.n_cols != nr_p_unsi) s1 +=(" Number of Columns of C input data frame must match number of pools! ");
    if (Nin_arma.n_cols != nr_p_unsi) s1 +=(" Number of Columns of N input data frame must match number of pools! ");   
    if (xi_arma.n_cols != nr_p_unsi) s1 +=(" Number of Columns of Xi input data frame must match number of pools! ");   
    if (strlen(s1.get_cstring())>10)
    {
        s1 +=("Number of pools defined as: ");
        s1 +=s2;   
    }    
    if (Cin_arma.n_rows != tseqlength_unsi) s3 +=(" Number of Rows of C input data frame must match number of time steps! ");
    if (Nin_arma.n_rows != tseqlength_unsi) s3 +=(" Number of Rows of N input data frame must match number of time steps! ");  
    if (xi_arma.n_rows != tseqlength_unsi) s3 +=(" Number of Rows of Xi input data frame must match number of time steps! ");  
    if (strlen(s3.get_cstring())>10)
    {
        s3 +=("Number of time steps defined as: ");
        s3 +=s4;   
    }          
        
    if ((!calcN) & (calcNbalance))  s3 +=(" Can't calculate N balance without N! ");
    s1+=s3;

    if (strlen(s1.get_cstring())>10) throw exception(s1.get_cstring());
    
    //////////////////////////////////////////////////////////////////////////////////////////////////////////
    //main part
    //////////////////////////////////////////////////////////////////////////////////////////////////////////

    NumericMatrix cpools(t_sim,nr_pools); 
    arma::mat cpools_arma = as<arma::mat>(cpools);
    
    NumericMatrix npools(t_sim,nr_pools); 
    arma::mat npools_arma = as<arma::mat>(npools);
    
    arma::mat cpools_sub = arma::zeros(1,nr_pools); 
    arma::mat npools_sub = arma::zeros(1,nr_pools); 
    
    arma::mat K1_sub(1,nr_pools); 
    arma::mat K2_sub(1,nr_pools); 
    arma::mat K3_sub(1,nr_pools); 
    arma::mat cn_sub(1,nr_pools); 

    // N mineralization variables
    NumericMatrix npools_diff(t_sim,3); 
    arma::mat npools_diff_arma = as<arma::mat>(npools_diff);
    NumericMatrix nmin(t_sim,nr_pools); 
    arma::mat nmin_arma = as<arma::mat>(nmin);
    NumericMatrix nmin2(t_sim,nr_pools); 
    arma::mat nmin2_arma = as<arma::mat>(nmin2);
    List nmin_i_list(nmin); 
    arma::cube nmin_i_cube = arma::zeros(t_sim,nr_pools,nr_pools);

    // start values for C and N        
    cpools_arma.row(0)=arma::trans(C0_arma);
    npools_arma.row(0)=arma::trans(N0_arma);
    
    // time loop
    for(int ts=1; ts<t_sim; ts++) 
    {  
        arma::rowvec xi_new_0 = xi_arma.row(ts-1);
        arma::rowvec xi_new_1 = xi_arma.row(ts);
        arma::rowvec K1(nr_pools);
        arma::rowvec K2(nr_pools);
        arma::rowvec K3(nr_pools);
        arma::rowvec K4(nr_pools);
        arma::rowvec K1n(nr_pools);
        arma::rowvec K2n(nr_pools);
        arma::rowvec K3n(nr_pools);
        arma::rowvec K4n(nr_pools);

        //////////////////////////////////////////////////////////////////////////////////////////////////////////
        //C-Pools calculation
        //////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        arma::rowvec Nin_here=Nin_arma.row(ts-1);
        arma::rowvec Cin_here=Cin_arma.row(ts-1);
    
        bool negatives=TRUE;
        int substeps=1;
        
        while (negatives)
        {
            arma::mat add_pools_sub= arma::zeros(substeps,nr_pools); 
            cpools_sub=join_vert(cpools_arma.row(ts-1),add_pools_sub);
            if (calcN) npools_sub=join_vert(npools_arma.row(ts-1),add_pools_sub);
            if (calcN) cn_sub=npools_sub;
            bool anynegatives=false;
            arma::rowvec xi_new_0_ss = xi_new_0;
            
                for(int ss=1; ss<substeps+1; ss++)
                {
                    if (ss>1) xi_new_0_ss=xi_new_1;
                    
                    //Runge Kutta 4th order, produces K1-K4 vectors, which elements correspond to the pools, their mean is calculated below 
                    K1=Cin_here/substeps + arma::trans((A_arma * arma::diagmat(xi_new_0_ss/timediv/substeps)  * arma::trans(cpools_sub.row(ss-1)))) ;   
                    K2=Cin_here/substeps + arma::trans((A_arma * arma::diagmat((xi_new_0_ss+xi_new_1)/2/timediv/substeps)  * arma::trans(cpools_sub.row(ss-1)+0.5*K1))) ; 
                    K3=Cin_here/substeps + arma::trans((A_arma * arma::diagmat((xi_new_0_ss+xi_new_1)/2/timediv/substeps)  * arma::trans(cpools_sub.row(ss-1)+0.5*K2))) ;  
                    K4=Cin_here/substeps + arma::trans((A_arma * arma::diagmat(xi_new_1/timediv/substeps)  * arma::trans(cpools_sub.row(ss-1)+K3))) ;
                    
                    if (ss==1) 
                    {
                        K1_sub=K1;
                        K2_sub=K2;
                        K3_sub=K3;
                    }
                    
                    if (ss>1)
                    {
                        K1_sub=join_vert(K1_sub, K1);
                        K2_sub=join_vert(K2_sub, K2);
                        K3_sub=join_vert(K3_sub, K3);
                    }
                                                                    
                    //mean of Runge-Kutta-Vestors, produces new C for each pool
                    cpools_sub.row(ss)=(K1+ 2*K2 + 2*K3 +K4)/6+cpools_sub.row(ss-1);

                    if (any(cpools_sub.row(ss)<0))
                    {
                        anynegatives=TRUE;
                        break;
                    }
                    
                    if (calcN) 
                    {
                        //////////////////////////////////////////////////////////////////////////////////////////////////////////
                        // N calculation  
                        //////////////////////////////////////////////////////////////////////////////////////////////////////////
                        
                        // deltaC = c.pools[ts-1,i] + c.In.here - c.pools[ts,i] = change in each c pool
                        // deltaN = n.pools[ts-1,i] + n.In.here - n.pools[ts,i] = change in each n pool
                        // deltaC/c.pools[ts,i] = deltaN/n.pools[ts,i]
                        // consequently:                    
                        npools_sub.row(ss) = (npools_sub.row(ss-1)+Nin_here/substeps) / ( (cpools_sub.row(ss-1)+Cin_here/substeps-cpools_sub.row(ss)) / cpools_sub.row(ss) + 1 );
                        
                        //c.In for pools exist where n.In == 0?
                        for(int i=0; i<nr_pools;i++)   
                        {   if (Cin_arma(ts-1,i)!=0){
                            if (Nin_arma(ts-1,i)==0) throw exception("C Input implies N input! There is C input without N input!") ;
                        }}
                        
                        //somewhere no C or no C input? (occurs if you start from 0 like with C-Tool_org, else rare case)
                        if (any(cpools_sub.row(ss)==0) | any(cpools_sub.row(ss-1)+Cin_arma.row(ts-1)==0)) 
                        {
                            // no C, or no C before and no Cinput
                            for(int i=0; i<nr_pools;i++) 
                            {if ((cpools_sub(ss,i)==0) | (cpools_sub(ss,i) + Cin_here(i) ==0 ) ) npools_sub(ss,i)=0;}

                            // there is C in pool (and so should N), but there have not been N or N input before (filled by transfer)
                            for(int i=0; i<nr_pools;i++) 
                            {if ((cpools_sub(ss,i)>0) & (npools_sub(ss-1,i)==0) & (Nin_here(i) ==0) )  
                                npools_sub(ss,i) = cpools_sub(ss,i)/ (cpools_sub(ss,0)/npools_sub(ss,0)) ;} //assume that first pools have C and N ... should work if first pool is always the first pool that gets input... worked so far
                        }
                        
                        //calculate cn     
                        for(int i=0; i<nr_pools;i++) 
                        {
                            if (npools_sub(ss,i)>0) cn_sub(ss,i) = cpools_sub(ss,i)/npools_sub(ss,i);
                            if (npools_sub(ss,i)==0) cn_sub(ss,i) =9999999;
                        }
                    }
                }
                
            if (anynegatives==TRUE) substeps=substeps*2;
            if (anynegatives==FALSE) negatives=false;
        }

        cpools_arma.row(ts)=cpools_sub.row(substeps);
        if (calcN) npools_arma.row(ts)=npools_sub.row(substeps);  
        
        ////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Nmin calculation - How much C is released by the single pools? Divide by target cn and get n released.  Runge Kutta 4th order
        ////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        if (calcN)
        {
            arma::mat nmin_sub = arma::zeros(substeps,nr_pools); 
            arma::rowvec xi_new_0_ss = xi_new_0;
            arma::mat nmin_i;
            
            arma::mat K1n_mat = arma::zeros(nr_pools,nr_pools); 
            arma::mat K2n_mat = arma::zeros(nr_pools,nr_pools); 
            arma::mat K3n_mat = arma::zeros(nr_pools,nr_pools); 
            arma::mat K4n_mat = arma::zeros(nr_pools,nr_pools); 
            
            for(int ss=1; ss<substeps+1; ss++)
            {
                    
                if (ss>1) xi_new_0_ss=xi_new_1;

                // Runge Kutta 4th order, produces K1n_mat-K4n_mat matrices, which rows correspond to the N source pools and which columns correspond to N sink pools,
                // and produces K1n-K4n vectors that are the colsums = how many N origins from which pool
                K1n_mat= A_arma *    arma::diagmat(  cpools_sub.row(ss-1)  % (xi_new_0_ss/timediv/substeps));  
                arma::inplace_trans(K1n_mat);
                K1n_mat= K1n_mat * arma::diagmat(1/cn_sub.row(ss));
                
                K2n_mat= A_arma *    arma::diagmat((  cpools_sub.row(ss-1) +0.5 * K1_sub.row(ss-1)) % ((xi_new_0_ss+xi_new_1)/2/timediv/substeps));                                 
                arma::inplace_trans(K2n_mat);
                K2n_mat= K2n_mat * arma::diagmat(1/cn_sub.row(ss));

                K3n_mat= A_arma *    arma::diagmat((  cpools_sub.row(ss-1) +0.5 * K2_sub.row(ss-1)) % ((xi_new_0_ss+xi_new_1)/2/timediv/substeps));                                 
                arma::inplace_trans(K3n_mat);
                K3n_mat= K3n_mat * arma::diagmat(1/cn_sub.row(ss));        
                
                K4n_mat= A_arma *    arma::diagmat((  cpools_sub.row(ss-1) + K3_sub.row(ss-1) ) % (xi_new_1/timediv/substeps));                                 
                arma::inplace_trans(K4n_mat);
                K4n_mat= K4n_mat * arma::diagmat(1/cn_sub.row(ss));
                
                arma::mat nmin_ss = -(K1n_mat + 2*K2n_mat + 2*K3n_mat + K4n_mat)/6; //N min of single pools
                arma::mat nmin_rowsum = arma::sum(nmin_ss,1);
                arma::inplace_trans(nmin_rowsum);
                nmin_sub.row(ss-1)=nmin_rowsum; //total N min
                
                for(int i=0; i<nr_pools;i++) 
                {
                    if (cn_sub(ss,i)==9999999) 
                    {
                        nmin_sub(ss-1,i)=0;
                        nmin_ss.row(i).fill(0);
                    }
                }
                    
                if(ss==1)nmin_i = nmin_ss;
                if(ss>1)nmin_i += nmin_ss;                               
                
            }
            arma::mat nminrow = arma::zeros(substeps,nr_pools);  
            nminrow = arma::sum(nmin_sub,0); // =rowSums

            nmin_arma.row(ts) = nminrow;  
            nmin2_arma.row(ts)=npools_arma.row(ts-1)+ Nin_arma.row(ts-1)- npools_arma.row(ts) ;     
            
            for(int i=0; i<nr_pools; i++) nmin_i_cube(arma::span(ts),arma::span::all,arma::span(i)) = nmin_i.row(i);
        }

        //////////////////////////////////////////////////////////////////////////////////////////////////////////
        // N balance calculation
        //////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        if (calcNbalance)
        {
            npools_diff_arma(ts,0)=arma::sum(npools_arma.row(ts-1))-arma::sum(npools_arma.row(ts));
            npools_diff_arma(ts,1)=arma::sum(Nin_here)+npools_diff_arma(ts,0)-arma::sum(nmin_arma.row(ts));
            npools_diff_arma(ts,2)=arma::sum(Nin_here)+npools_diff_arma(ts,0)-arma::sum(nmin2_arma.row(ts));
        }
    }
    
            
    //////////////////////////////////////////////////////////////////////////////////////////////////////////
    //create output
    //////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    //define list output names
    CharacterVector list_names = {"C"};

    CharacterVector pool_i_names;

    for(int i=0; i<nr_pools; i++) 
    { 
        String pool_i_name("pool_");
        pool_i_name+=i+1;
        pool_i_names.push_back(pool_i_name);
    }
    
    //define length of output list
    int listlength = 1; //c always out
    if (calcN) listlength+=(nr_pools+3);
    if (calcNbalance) listlength+=1;
    List listout (listlength); 
    
    cpools=wrap(cpools_arma);
    colnames(cpools)=pool_i_names;

    listout(0)=cpools;
    
    if (calcN)
    {
        int listrunner = 4;
        npools=wrap(npools_arma);
        nmin=wrap(nmin_arma);
        nmin2=wrap(nmin2_arma);
        colnames(npools)=pool_i_names;
        colnames(nmin)=pool_i_names;
        colnames(nmin2)=pool_i_names;
        listout(1)=npools;
        listout(2)=nmin2;
        listout(3)=nmin;
        list_names.push_back("N");
        list_names.push_back("Nloss");
        list_names.push_back("Nmin");


        for(int i=0; i<nr_pools; i++) 
        {
            NumericMatrix a = wrap(nmin_i_cube.slice(i));
            colnames(a)=pool_i_names;
            listout(listrunner)= a ;
            listrunner+=1;
            String nmin_i_name("Nmin.sink.");
            nmin_i_name+=i+1;
            list_names.push_back(nmin_i_name);
        }
    
        if (calcNbalance)   
        {
            npools_diff=wrap(npools_diff_arma);
            CharacterVector npool_diff_names={"d_N","N_bal1","N_bal2"};
            colnames(npools_diff)=(npool_diff_names);
            listout(listrunner)=npools_diff;
            list_names.push_back("Nbalance");
        }
    }
    
    listout.names()=list_names;
    return listout;
}

