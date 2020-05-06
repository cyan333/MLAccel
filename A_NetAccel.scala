import spatial.dsl._

@spatial object A_NetAccel extends SpatialApp {


    /**
     * eq_region
     *
     * Utility function to assist in testing. Checks a subset of a host matrix against another
     */
    /*def eq_region(
        a_addr_x: I32,
        a_addr_y: I32,
        b_addr_x: I32,
        b_addr_y: I32,
        len_x: I32,
        len_y: I32,
        mat_a: Matrix[T],
        mat_b: Matrix[T]): Boolean = {
            var y: I32 = 0
          var x: I32 = 0
            var valid: Boolean = true
            for (y <- 0 until (len_y)){
                for (x <- 0 until (len_x)){
                    val a_y = y + a_addr_y
                    val a_x = x + a_addr_x
                    val b_y = y + b_addr_y
                    val b_x = x + b_addr_x
                    if (mat_a(a_y,a_x) == mat_b(b_y,b_x) != true){
                        valid = false;
                        println("Mismatch on " + a_y + "," + a_x + "::" + b_y + ","+b_x + " " + mat_a(a_y,a_x) + " != " + mat_b(b_y,b_x))
                    }
                }
            }

            valid
    }*/


    def main(args: Array[String]): Unit = {

        ///////////////////////////////////////////////
        // CONFIG
        ///////////////////////////////////////////////

        type T = FixPt[TRUE,_16,_16]
	//type T = Int
        // YOUR CONFIGURATION HERE

        //
        // SRAM CONFIG
        //
        //
        // MODIFY TO BE WITHIN FPGA LIMITS
        // SPECIFIC FPGA IS: XILINX VU9P
        // YOU DECIDE THE CONFIG BASED ON YOUR SCHEDULE
        val GLOBAL_SRAM_WEIGHT_M = 4000
        val GLOBAL_SRAM_WEIGHT_N = 4000

        val GLOBAL_SRAM_ACT_M = 4000
        val GLOBAL_SRAM_ACT_N = 4000
	//val bm = 16 //16
	//val bn = 784  //784
	//val bp = 9  //9
	val c_init = (0::60, 0::60){(_,_) => 0.to[T] }
	
        //
        // DRAMs
        
        val CFG_DRAM_WEIGHT_MEM_M = 4000
        val CFG_DRAM_WEIGHT_MEM_N = 4000

        val CFG_DRAM_INPUT_MEM_M = 32
        val CFG_DRAM_INPUT_MEM_N = 32

        val CFG_DRAM_OUTPUT_MEM_M = 64
        val CFG_DRAM_OUTPUT_MEM_N = 64

        

	//var a = ArgIn[Int]
	//var b = ArgIn[Int]
	//setArg(a, 0)
	//setArg(b, 0)
        //
        // INPUT DATA
        //
        //val imcol = HostIO[Int]
        //setArg(imcol,0)
        // Load all the inputs you need here into indvidual variables
        
        val input_batch = loadCSV2D[T]("test0.csv") 
//: Matrix[T]
        val layer1 = loadCSV2D[T]("conv1.csv")
        val layer2 = loadCSV2D[T]("conv2.csv")
	val layer3 = loadCSV2D[T]("lin1.csv")
	val layer4 = loadCSV2D[T]("lin2.csv")
 //: Matrix[T]
	
        ///////////////////////////////////////////////
        // HOST DRAM
        ///////////////////////////////////////////////


        // Now we need to put all of the weights into host weight memories
        // because partial copies are annoying and we may have gaps
        // This strategy iteratres over all of the locations in the host weight
        // memory and puts the correct value in each location or zero if
        // nothing is defined for that location
        
	/*val HOST_weight_mem = Matrix.tabulate(CFG_DRAM_WEIGHT_MEM_M, CFG_DRAM_WEIGHT_MEM_N){(y,x) =>
		if( y >= 0 && y < 784) {
			if(x >= 0 && x < 3136){
				layer3(y,x)
		}
			else {
				0.to[T]
		}
	}
		else if( y>= 784 && y < 794) {
			if(x >= 0 && x < 784) {
		
				layer4(y-784,x)
		}
			else {
				0.to[T]
		}

	}
		
		else if( y>= 794 && y < 3866) {
			if(x >= 0 && x < 3) {
			
				layer2(y-794,x)
		}
			else if(x >=3 && x < 6) {
				if(y < 842) {
				layer1(y-794,x-3)
				}
				else {0.to[T]}
			}
			else {
				0.to[T]
			}
			}
		else { 0.to[T]}
	
	}*/
	
	/*val HOST_weight_mem = Matrix.tabulate(CFG_DRAM_WEIGHT_MEM_M, CFG_DRAM_WEIGHT_MEM_N){(x,y) => 
	if( y>=0 && y < 3136) {
		if( x>=0 && x < 784) {
			layer3(x,y)
		}
	}
	}*/


val HOST_weight_mem = Matrix.tabulate(CFG_DRAM_WEIGHT_MEM_M, CFG_DRAM_WEIGHT_MEM_N){(x,y) =>
            if( y >= 0 && y < 3 ){ //you can configure these values to set where to place
                                      //the data
                if ( x >= 0 && x < 48 ){
                    layer1(x,y) //note: will need to add an offset term if not zero indexed
                } else {
                    0.to[T]
                }
            }
	    else if( y >=3 && y < 6 ) {
		if ( x >=0 && x < 3072) {
		     layer2(x,y-3)
		}
		else {
			0.to[T]
		}
		}
	   else if( y >=6 && y < 3142 ) {
		if ( x >=0 && x < 784 ) {
		     layer3(x,y-6)
		}
		else {
			0.to[T]
		}
		}
	    else if( y >=3142 && y < 3926 ) {
		if ( x >=0 && x < 10 ) {
		      layer4(x,y-3142)
		}
		else {
			0.to[T]
		}
   		}
	    else {
			0.to[T]
		}	 
     	}

        //We do the same thing as above, but for our input batch
        val HOST_input_mem = Matrix.tabulate(CFG_DRAM_INPUT_MEM_M, CFG_DRAM_INPUT_MEM_N){(y,x) => 
           //To change
             if (y >=0 && y < 30){
                if (x >= 0 && x < 30){
                    input_batch(y,x)
                } else {
                    0.to[T]
                }
                }
	     else {
		    0.to[T]
		}
        }
        //To change 
        //val gold = (0::32,0::32){foreach (i <- 0 until 32){

				//foreach (j <- 0 until 32){
					
					//Array.tabulate(32) {k => HOST_weight_mem(i,k) * HOST_input_mem(k,j)}.reduce{_+_}}}} 
	
        //print (gold)
	val gold1 = (0::8, 0::8){(i,j) => 
            Array.tabulate(8){k => HOST_input_mem(i,k) * HOST_weight_mem(k,j)}.reduce{_+_}}
	println(r"expected cksum: ${gold1.map(a => a).reduce{_+_}}")
	print("Done the best")

        val HOST_output_init = Matrix.tabulate(CFG_DRAM_OUTPUT_MEM_M, CFG_DRAM_INPUT_MEM_N){(y,x) => 0.to[T]}
	var max_value = 0


        ///////////////////////////////////////////////
        // ACCEL DRAM
        ///////////////////////////////////////////////



        val DRAM_input_mem = DRAM[T](CFG_DRAM_INPUT_MEM_M, CFG_DRAM_INPUT_MEM_N)
        val DRAM_weight_mem = DRAM[T](CFG_DRAM_WEIGHT_MEM_M, CFG_DRAM_WEIGHT_MEM_N)
        val DRAM_output_mem = DRAM[T](CFG_DRAM_OUTPUT_MEM_M, CFG_DRAM_OUTPUT_MEM_N)




        ///////////////////////////////////////////////
        // HOST SETUP
        ///////////////////////////////////////////////


        //Do the HOST -> DRAM Copy
        setMem(DRAM_weight_mem, HOST_weight_mem)
        setMem(DRAM_input_mem, HOST_input_mem)

        //Set output mem to random output values for testing
        setMem(DRAM_output_mem, HOST_output_init)

	val C = DRAM[T](4000,4000)
        setMem(C, c_init)
        ///////////////////////////////////////////////
        // THE ACCELERATOR
        ///////////////////////////////////////////////

        Accel {

            // M Rows, N Cols 
            // M --> Y
            // N --> X

            ///////////////////////////////////////////////
            // SRAM GLOBAL BUFFERS
            ///////////////////////////////////////////////


            //global sram buffer is large to support writes and reads
            //one side while inputs are read from the other
            val SRAM_weight_mem = SRAM[T](GLOBAL_SRAM_WEIGHT_M, GLOBAL_SRAM_WEIGHT_N)
            val SRAM_act_mem    = SRAM[T](GLOBAL_SRAM_ACT_M, GLOBAL_SRAM_ACT_N*2)



            ///////////////////////////////////////////////
            // DATA TRANSFER FUNCTIONAL BLOCKS
            ///////////////////////////////////////////////


            /**
             *  generic_dram_sram_copy
             *
             *  dram -> sram copy from a dram2 to a sram2
             */
            def generic_dram_sram_copy(
                src_dram: DRAM2[T],
                dst_sram: SRAM2[T],
                dram_idx_x: Int, //src
                dram_idx_y: Int, //src
                sram_idx_x: Int, //dst
                sram_idx_y: Int, //dst
                len_x: Int,
                len_y: Int,
                ) : Unit = {
                    val target_sram_region = dst_sram(
                        sram_idx_y::sram_idx_y+len_y,
                        sram_idx_x::sram_idx_x+len_x
                    )

                    val target_dram_region = src_dram(
                        dram_idx_y::dram_idx_y+len_y,
                        dram_idx_x::dram_idx_x+len_x
                    )

                    target_sram_region.load(target_dram_region)
			//print("Value at 3,4 for activation")
		//print(SRAM_act_mem(3,4))
		//print("Value at 3,4 for weights")
		//print(SRAM_weight_mem(3,4))
            }

            /**
             *  generic_sram_dram_copy
             *
             *  sram -> dram copy from a sram2 to a dram2
             */
            def generic_sram_dram_copy(
                src_sram: SRAM2[T],
                dst_dram: DRAM2[T],
                sram_idx_x: Int, //src
                sram_idx_y: Int, //src
                dram_idx_x: Int, //dst
                dram_idx_y: Int, //dst
                len_x: Int,
                len_y: Int
            ) : Unit = {
                val target_sram_region = src_sram(
                    sram_idx_y::sram_idx_y+len_y,
                    sram_idx_x::sram_idx_x+len_x
                )

                val target_dram_region = dst_dram(
                    dram_idx_y::dram_idx_y+len_y,
                    dram_idx_x::dram_idx_x+len_x
                )

                target_dram_region.store(target_sram_region)
            }


            /** 
             *  copy_sram_sram
             *
             *  Copies a region of memory from sram2 -> sram2
             */
            def copy_sram_sram(
                SRAM_src: SRAM2[T],
                SRAM_src_idx_x: Int,
                SRAM_src_idx_y: Int,
                SRAM_dst: SRAM2[T],
                SRAM_dst_idx_x: Int,
                SRAM_dst_idx_y: Int,
                len_x: Int,
                len_y: Int
            ) : Unit = {

                Sequential.Foreach(0 until len_y by 1,
                    0 until len_x by 1)
                    {(y, x) =>
                        SRAM_dst(SRAM_dst_idx_y+y,SRAM_dst_idx_x+x) = SRAM_src(SRAM_src_idx_y+y,SRAM_src_idx_x+x)
                    }
            }
	
            ///////////////////////////////////////////////
            // FUNCTIONAL UNITS
            ///////////////////////////////////////////////
		            /**
             * blocked_mxm_op
             *
             * blocked MxM from the weight SRAM and act SRAM
             */
   
          def blocked_mxm_op(
                weight_idx_x: Int,
                weight_idx_y: Int,
                weight_N: Int, //9
                weight_M: Int, //16
                act_idx_x: Int, 
                act_idx_y: Int,
                act_M: Int, //784
                act_B: Int, //9
                output_idx_x: Int,
                output_idx_y: Int,
	        bm: Int, //16
	        bn: Int,  //784
	        bp: Int  //9

            ) : Unit = {
		val output_idx_x1 = Reg[Int](0)
		output_idx_x1 := output_idx_x
		val output_idx_y1 = Reg[Int](0)
		output_idx_y1 := output_idx_y
                val act_idx_y1 = Reg[Int](0)
                act_idx_y1 := act_idx_y
                val tileC = SRAM[T](bm,bn)
                //println("bm = " + bm + "  bn = " + bn + "  bp = " + bp) 
		Sequential.Foreach(weight_idx_y until weight_idx_y+weight_M by bm par 1) { i =>
		Sequential.Foreach(act_idx_x until act_idx_x+act_M by bn par 1) { j =>
                  tileC load C(i::i+bm, j::j+bn)
                  // Accumulate on top of C tile over all tiles in P dimension
                  Sequential.MemFold(tileC)(weight_idx_x until weight_idx_x+weight_N by bp) { k =>
                    val tileA = SRAM_weight_mem(i::i+bm, k::k+bp)
                    val tileB = SRAM_act_mem(act_idx_y1-weight_idx_x+k::act_idx_y1-weight_idx_x+k+bp, j::j+bn) 
                    val accum = SRAM[T](bm, bn)
                    /*
                    Sequential.Foreach(bm by 1, bp by 1){ (a,b) =>
                      println("tileA --> (" + (i+a) + "," + (k+b) + ") --> " + SRAM_weight_mem(a+i,b+k))
                    }
                      println("------------------------")
                    Sequential.Foreach(bn by 1, bp by 1){ (d,c) =>
                      println("tileB --> (" + (act_idx_y1-weight_idx_x+c+k) + "," + (d+j) + ") --> " + SRAM_act_mem(act_idx_y1-weight_idx_x+c+k,j+d))
                    }
                    */
                    /*
                    Sequential.Foreach(bm by 1, bp by 1){ (a,b) =>
                      println("tileA --> (" + (a) + "," + (b) + ") --> " + tileA(a,b))
                    }
                      println("------------------------")
                    Sequential.Foreach(bn by 1, bp by 1){ (d,c) =>
                      println("tileB --> (" + (c) + "," + d + ") --> " + tileB(c,d))
                    }
                    */

                    // Perform matrix multiply on tile
                    Sequential.MemReduce(accum)(bp by 1 par 1){ (kk) =>
                      val tileC_partial = SRAM[T](bm,bn)
                      Sequential.Foreach(bm by 1 par 1, bn by 1 par 1){ (ii,jj) =>

                        tileC_partial(ii,jj) = tileA(ii,kk) * tileB(kk,jj)
                       // println("xxxxxxxxxxxxxxxxxxxxxxxx")
                //println("tileA ---- (" + (ii) + "," + (kk) + ") -->" + tileA(ii,kk))
                //println("tileB ---- (" + (kk) + "," + (jj) + ") -->" + tileB(kk,jj))
                //println("tileC_partial ---- (" + (ii) + "," + (jj) + ") -->" + tileC_partial(ii,jj))
                      }
                      tileC_partial
                    }{_+_}
                  }{_+_}
                 /* 
                    Sequential.Foreach(0 until bm by 1, 0 until bn by 1){(i,j) =>
                      print("--------Tile CCCC-------")
                      println("(" + i + "," + j + ") ---> " + tileC(i,j))
                    }
                    */
                    //print("output_idx_y1 == " + output_idx_y1 + "--------output_idx_x1 == " + output_idx_x1)
                    copy_sram_sram(
                      tileC,
                      0.to[Int],
                      0.to[Int],
                      SRAM_act_mem,
                      output_idx_x1,
                      output_idx_y1,
                      bn,
                      bm )
                    //C(i::i+bm,j::j+bn) store SRAM_act_mem(output_idx_y1::output_idx_y1+bm,output_idx_x1::output_idx_x1+bn)

                    //Sequential.Foreach(bm by 1, bn by 1){ (i,j) =>
                      //println("final SRAM Result (" + (output_idx_y1+i) + "," + (output_idx_x1+j)+ ") -->" + SRAM_act_mem(output_idx_y1+i,output_idx_x1+j))
                    //}

                }
                  //println("output_idx_x1 ====" + output_idx_x1)
                  output_idx_x1 :+= bn
                }
                
                //println("output_idx_x1 ====" + output_idx_x1)
		output_idx_x1 := 0
		output_idx_y1 :+= bm
              }

              
              /**
             * maxpool2D_k2
             *
             * Maxpool from act sram to act sram with kernel size = 2
             */
/*
            def maxpool2D_k2(
              src_addr_x: Int,
              src_addr_y: Int,
              dst_addr_x: Int,
              dst_addr_y: Int,
              len_x: Int,
              len_y: Int
            ) : Unit = {
		
			val b = Reg[Int](0)
			val c = Reg[Int](0)
			val a = Reg[Int](0)
			a := 0
			b := 0
			c := 0
			SRAM_act_mem(63,63) = 0
			val temp0 = Reg[T](0)
			print("SRAM act mem max value")
			
			Foreach (0 until len_y by 2, 0 until len_x by 2){(i,j) => 
				SRAM_act_mem(127,127) = SRAM_act_mem(i+src_addr_y,j+src_addr_x)
                          Foreach (0 until 2 by 1, 0 until 2 by 1) {(y,x) =>
                            if(SRAM_act_mem(i+src_addr_y+y,j+src_addr_x+x) > SRAM_act_mem(63,63)) { temp0 = SRAM_act_mem(i+src_addr_y+y,j+src_addr_x+x)}
                          }
                          SRAM_act_mem(dst_addr_y+a,dst_addr_x+b) = temp0
                          if((a <= (len_x/2 -1)) && (b < (len_y/2-1))){
                            b = b+1
                          }
                          else {
                            b := dst_addr_y
                            a :+= 1
                          }
                        }
			
			print("///////Max pooling values////////")
			Foreach (0 until len_y/2 by 1, 0 until len_x/2 by 1){(y,x) => 
			  print(SRAM_act_mem(dst_addr_y+y,dst_addr_x+x))}
	 
					
}	              // Your implementation here

          */
            def maxpool2D_k2(
              src_addr_x: Int,
              src_addr_y: Int,
              dst_addr_x: Int,
              dst_addr_y: Int,
              len_x: Int,
              len_y: Int
            ) : Unit = {

              // Your implementation here
                Sequential.Foreach(0 until len_y by 2, 0 until len_x by 2){(i,j) =>
                        val maxtop = max(SRAM_act_mem(src_addr_y + i, src_addr_x + j), SRAM_act_mem(src_addr_y + i, src_addr_x + j + 1))
                        val maxbot = max(SRAM_act_mem(src_addr_y + i + 1, src_addr_x + j), SRAM_act_mem(src_addr_y + i + 1, src_addr_x + j + 1))
                        val maxOut = max(maxtop,maxbot)
                        SRAM_act_mem(dst_addr_y + i/2, dst_addr_x + j/2) = maxOut
                        //println("max pooling --> (" + (dst_addr_y + i/2) + "," + (dst_addr_x + j/2) + ")  max output = " + maxOut)
                }	

            }



            /**
             * relu1D
             *
             * Relu a 1d array in the activation SRAM
             */
            def relu1D(
                src_addr_x: Int,
                src_addr_y: Int,
                dst_addr_x: Int,
                dst_addr_y: Int,
                len_x: Int,
                ) : Unit = {
			              // Your implementation here


            }

            /**
             * relu2D
             *
             * Relu a 2d array in the activation SRAM
             */
            def relu2D(
                src_addr_x: Int,
                src_addr_y: Int,
                dst_addr_x: Int,
                dst_addr_y: Int,
                len_x: Int,
                len_y: Int,
                ) : Unit = {
                  Sequential.Foreach (0 until len_y by 1,0 until len_x by 1){(y,x) => 
                    if(SRAM_act_mem(src_addr_y+y,src_addr_x+x) < 0) 
                            { SRAM_act_mem(dst_addr_y+y,dst_addr_x+x) = 0 } 
                    else { SRAM_act_mem(dst_addr_y+y,dst_addr_x+x) = SRAM_act_mem(src_addr_y+y,src_addr_x+x)}}

	

              // Your implementation here

            }

            ///////////////////////////////////////////////
            // RESHAPE FUNCTIONS
            ///////////////////////////////////////////////

            /**
             * flatten
             *
             * Flattens a submatrix in the activation SRAM
             */
            def flatten(
                src_addr_x: Int,
                src_addr_y: Int,
                dst_addr_x: Int,
                dst_addr_y: Int,
                len_x: Int,
                len_y: Int
            ) : Unit = {
			val flat = Reg[Int](0)
			flat := 0
			val temp_reg = Reg[T](0)
			println("+++++++Flattening+++++++")
			Sequential.Foreach (0 until len_y by  1, 0 until len_x by 1){(i,j) => 
                          temp_reg = SRAM_act_mem(src_addr_y+i,src_addr_x+j)
                          SRAM_act_mem(dst_addr_y+flat,dst_addr_x) = temp_reg
                          //println("(" + (dst_addr_y+flat) + "," + (dst_addr_x) + ") --> " + SRAM_act_mem(dst_addr_y+flat,dst_addr_x) )
                          flat:+= 1
                        }

			//Foreach (0 until len_y by 1,0 until len_x by 1){(y,x) => 
					//print(SRAM_act_mem(dst_addr_y+y,dst_addr_x+x))}
			println("flattening Done") 
			
			
              // Your implementation here

            }

	/*def transpose(
                src_addr_x: Int,
                src_addr_y: Int,
                dst_addr_x: Int,
                dst_addr_y: Int,
                len_x: Int,
                len_y: Int
            ) : Unit = {
	        Foreach(0 until len_y by 1, 0 until len_x by 1){(i,j) =>
		    SRAM_weight_mem(dst_addr_y + j, dst_addr_x + i) = SRAM_weight_mem(src_addr_y + i, src_addr_x + j)
		}
            }*/
		

            /**
             * im2col_b3
             *
             * Rearrange 3x3 blocks of the input into columns
             */
            def im2col_b3(
              src_addr_x: Int,
              src_addr_y: Int,
              dst_addr_x: Int,
              dst_addr_y: Int,
              len_x: Int, //16
              len_y: Int, //256
	      image_length: Int //16
            ) : Unit = {
			val imcol = Reg[Int](0)
			val imrow = Reg[Int](0)
                        //imrow := 0
			val temp = Reg[T](0)
			val imrow_channel = Reg[Int](0)

			println("IM2COL START")
			imrow_channel := 0

			/*Foreach(1 until number_of_filters by 1){(num) => 
			Foreach (0 until len_y by 1, 0 until len_x by 1) {(a,b) => 
			SRAM_act_mem(src_addr_y + a + (num*len_y), src_addr_x + b) = SRAM_act_mem(src_addr_y + a, src_addr_x + b)
}
}*/
                        
			Sequential.Foreach(0 until len_y by image_length){c =>
			
                          //Pass the parameter of image-length as image width
                          Sequential.Foreach (0 until image_length-2 by 1, 0 until len_x-2 by 1){(i,j) =>
                            //println("i = " + i + "   j = " + j)
                            Sequential.Foreach (0 until 3 by 1, 0 until 3 by 1){(a,b) =>
                                    temp = SRAM_act_mem(src_addr_y+c+i+a, src_addr_x+j+b)
                                    //println("reading from --> ("+ (src_addr_y+c+i+a)+ "," + (src_addr_x+j+b) + ") --> " + temp)
                                    /*if(i==0){
                                    print("(" + a+ "," + b + ")" + "(" + i + "," + j +")")
                                    println(temp)
                                    print("(" + imrow + "," + imcol + ")")
                                    println("     addr_y" + dst_addr_y + "dst_addr_x = " + dst_addr_x)
                                    }*/
                                    SRAM_act_mem(dst_addr_y+imrow,dst_addr_x+imcol) = temp
                                    //print("----------im2col activation-----------")
                                    //print("save to --------  (" + (dst_addr_y+imrow) + "," + (dst_addr_x+imcol) + ")  --->")  

                                    //println(SRAM_act_mem(dst_addr_y+imrow,dst_addr_x+imcol))

                                    imrow :+= 1

                                    if(imrow % (9*imrow_channel+9) == 0) {
                                            imrow := imrow_channel*9+0
                                            imcol :+= 1
                                    }
                            }
                            //println("********im2col b2 Next **********")
                          }
					imrow :+= 9
                                        imcol := 0
                                        imrow_channel :+= 1
				
                        }

			print("IM2COL END")
			print("Number of rows")
			print(imrow)
			print("Number of columns")
			println(imcol)
     }    
   
            def im2col_filter(
              src_addr_x: Int,
              src_addr_y: Int,
              dst_addr_x: Int,
              dst_addr_y: Int,
              len_x: Int,
              len_y: Int,
	      image_depth_mul_kernel: Int
            ) : Unit = { 
			val imcol_filter = Reg[Int](0)
			val imrow_filter = Reg[Int](0)
			imcol_filter := 0
			//val temp1 = Reg[T](0)	
			Sequential.Foreach (0 until len_y by image_depth_mul_kernel){b =>
			imrow_filter := 0 
                          Sequential.Foreach(0 until image_depth_mul_kernel by 3){a =>
                            Sequential.Foreach(0 until 3 by 1, 0 until 3 by 1) {(i,j) => 
                              //temp1 = SRAM_weight_mem(src_addr_y+i+a+b,src_addr_x+j)
                              //println(" -------------------- ")
                              //println("a = " + a + "----- b =" + b +"(" + i + " ," + j + ")")
                              //print("temp1 = ")
                              //println(temp1)
                              SRAM_weight_mem(dst_addr_y+imcol_filter,dst_addr_x+imrow_filter) = SRAM_weight_mem(src_addr_y+i+a+b,src_addr_x+j)
                              //print("-------im2col filter------")
                              //print("save to --------  (" + (dst_addr_y+imcol_filter) + "," + (dst_addr_x+imrow_filter) + ")")
                              //println(SRAM_weight_mem(dst_addr_y+imcol_filter,dst_addr_x+imrow_filter))
                              imrow_filter :+= 1
                            }
                          }
                              //println("-----im2col Filter Next-----")
                              imcol_filter :+= 1
                        }
                  }
	
             /* col2im_b3
             *
             * Rearrange columns into the output image*/
             
            def col2im_b3(
              src_addr_x: Int,
              src_addr_y: Int,
              dst_addr_x: Int,
              dst_addr_y: Int,
              len_x: Int,
              len_y: Int,
	      param_random: Int
            ) : Unit = {
			
                println("///////////COL2IM starts//////////")
                val i1 = Reg[Int](0)
                i1 := 0
                val j1 = Reg[Int](0)
                val k1 = Reg[Int](0)
                val temp2 = Reg[T](0)
                val trace = Reg[Int](0)
                val j = Reg[Int](0)
                k1 := 0
                j := 0
                Sequential.Foreach(0 until len_y by 1) { i =>
                  Sequential.Foreach(0 until len_x by param_random){ col_group =>
                    Sequential.Foreach(0 until param_random by 1) { col_inside_group =>
                      temp2 = SRAM_act_mem(src_addr_y+i,src_addr_x+col_group+col_inside_group)
                      SRAM_act_mem(dst_addr_y+j+(i*param_random),dst_addr_x+col_inside_group) = temp2
                      //println("col2cim: index (" + (dst_addr_y+j+(i*param_random)) + "," + (dst_addr_x+col_inside_group) + ") --> " + SRAM_act_mem(dst_addr_y+j+(i*param_random),dst_addr_x+col_inside_group))
                    }
                    j :+= 1	
                  }
                  j := 0
                }	
                //print("THE VALUE at 23,2 IS")
                //println(SRAM_act_mem(23,2))			
          }

          def padding(
		src_addr_x: Int,
		src_addr_y: Int,
		dst_addr_x: Int,
		dst_addr_y: Int,
		len_x: Int,
		len_y: Int,
		image_length: Int
		): Unit = {
		val temp_pad = Reg[T](0)
		val shift = Reg[Int](0)
		shift := 0
		val dummy = Reg[Int](0)
		Sequential.Foreach(0 until len_y by image_length){ i =>
				
			Sequential.Foreach(-1 until image_length+1 by 1) { j =>
			Sequential.Foreach(-1 until len_x+1 by 1) { k =>
				if(j == -1) {
					SRAM_act_mem(dst_addr_y+shift+j+1,dst_addr_x+k+1) = 0
					//print("First row")
					//println(SRAM_act_mem(dst_addr_y+shift+j+1,dst_addr_x+k+1))
					//shift :+= 1
				}
				else if(j == image_length){
					
					SRAM_act_mem(dst_addr_y+shift+j+1,dst_addr_x+k+1) = 0
					//print("Final row")
					//println(SRAM_act_mem(dst_addr_y+shift+j+1,dst_addr_x+k+1))
					//dummy := 0	
				}
				else if(k == -1) {
					SRAM_act_mem(dst_addr_y+shift+j+1,dst_addr_x+k+1) = 0
					//print("First column")
					//println(SRAM_act_mem(dst_addr_y+shift+j+1,dst_addr_x+k+1))
				}
				else if(k == len_x) {
					SRAM_act_mem(dst_addr_y+shift+j+1,dst_addr_x+k+1) = 0
					//print("Last column")
					//println(SRAM_act_mem(dst_addr_y+shift+j+1,dst_addr_x+k+1))
				}
				else {
					temp_pad = SRAM_act_mem(src_addr_y+i+j,src_addr_x+k)
					//print("Temp_pad")
					//print(temp_pad)
			 		SRAM_act_mem(dst_addr_y+shift+j+1,dst_addr_x+k+1) = temp_pad
					//print("Other elements")
					//println(SRAM_act_mem(dst_addr_y+shift+j+1,dst_addr_x+k+1))	
				}
				
				}
			}
				shift :+= image_length+2
		}
				//Foreach (0 until len_y+2 by 1, 0 until len_x+2 by 1){(i,j) =>
                                //println("padding index = (" + i + "," + j + ") ==>" + SRAM_act_mem(dst_addr_y+i, dst_addr_x+j))}
	  }


            ///////////////////////////////////////////////
            // TESTING
            ///////////////////////////////////////////////

            //YOUR TESTING FUNCTIONS HERE

            ///////////////////////////////////////////////
            // NETWORKS
            ///////////////////////////////////////////////

            /**
             * forward
             *
             * Implements the schedule for the conv net
             */
            def forward() : Unit = {
		//val dram_idx_x = 0 //src
                //val dram_idx_y = 0 //src
                //val sram_idx_x = 0  //dst
                //val sram_idx_y = 0 //dst
                //val len_x = 32
                //val len_y = 32
               generic_dram_sram_copy(DRAM_input_mem : DRAM2[T],
                SRAM_act_mem: SRAM2[T],
                0 :Int, //src
                0 :Int, //src
                0 :Int,  //dst
                0 :Int,//dst
                30 :Int, 
                30 :Int)
		generic_dram_sram_copy(DRAM_weight_mem : DRAM2[T],
		SRAM_weight_mem : SRAM2[T],
		0 :Int,
		0 :Int,
		0 :Int,
		0 :Int,
		4000 :Int,
		4000 :Int)
		im2col_b3(
                  0: Int,
                  0: Int,
                  0: Int,
                  30: Int,
                  30: Int,
                  30: Int,
                  30: Int
                )
              /*
                Foreach(0 until 784 by 1, 0 until 9 by 1){ (i,j) =>
                  if(j==0) {
                    println(" ^^^^^^^^^^^ im2col b2 function call ^^^^^^^^^^  ")
                  }
                  println("read from ******* (" + j + "," + i + ") --> " + SRAM_act_mem(30+j,i))
                }*/
                im2col_filter(
                  0: Int,
                  0: Int,
                  0: Int,
                  3072: Int,
                  3: Int, 
                  48: Int,
                  3: Int
                )/*
                Foreach(0 until 16 by 1, 0 until 9 by 1){ (i,j) =>
                  if(j==0) {
                    println(" --------function call ---------- ")
                  }
                  println(" read from ---------( " + (3072+i) + "," + j + ")")
                  println(SRAM_weight_mem(3072+i,j))
                }*/
               println("layer1 mxm")
                blocked_mxm_op(
                  0: Int,
                  3072: Int,
                  9: Int,
                  16: Int,
                  0: Int,
                  30: Int,
                  784: Int,
                  9: Int,
                  0: Int, //mxm result des x
                  0: Int,
                  16:Int,
                  784:Int,
                  9: Int //mxm result des y
                )
                /*
                println("++++++++++Padding++++++++++++")
                Foreach(0 until 16 by 1, 0 until 784 by 1){ (i,j) =>
                  if(j==783) {
                    print(SRAM_act_mem(i,j))
                    println("")
                  }
                  else {
                    print(SRAM_act_mem(i,j) + ",")
                  }
                }
*/
                col2im_b3(
                  0: Int,
                  0: Int,
                  0: Int, 
                  16: Int,
                  784: Int,
                  16: Int,
                  28: Int
                ) 
               /* 
                Foreach(0 until 28 by 1, 0 until 28 by 1){ (i,j) =>
                  if(i==0) {
                    println(" ^^^^^^^^^^^ col2im function call ^^^^^^^^^^  ")
                  }
                  println("read from ******* (" + j + "," + i + ") --> " + SRAM_act_mem(16+j,i))
                }
                */
                relu2D(
                  0: Int,
                  16: Int,
                  0: Int,
                  16: Int, 
                  28: Int,
                  448: Int
                )

                maxpool2D_k2(
                  0: Int,
                  16: Int,
                  0: Int,
                  0: Int,
                  28: Int,
                  448: Int
                )
                //println("------MAX Pooling--------")
                //Foreach(0 until 224 by 1, 0 until 14 by 1){ (i,j) =>
                //  println("max pooling result ******* (" + j + "," + i + ") --> " + SRAM_act_mem(j,i))
                //}

                padding(
		  0: Int,
		  0: Int,
		  0: Int,
		  224: Int,
		  14: Int,
		  224: Int, 
		  14: Int 
		)
                /*
                println("++++++++++Padding++++++++++++")
                Foreach(0 until 256 by 1, 0 until 16 by 1){ (i,j) =>
                  //println("padding result ----> at (" + i + "," + j + ") --> " + SRAM_act_mem(224+i,j))
                  if(j==15) {
                    print(SRAM_act_mem(224+i,j))
                    println("")
                  }
                  else {
                    print(SRAM_act_mem(224+i,j) + ",")
                  }
                }
                */
                im2col_b3(
                  0: Int,
                  224: Int,
                  0: Int,
                  0: Int,
                  16: Int,
                  256: Int,
                  16: Int
                )
                im2col_filter(
                  3:Int,
                  0:Int,
                  0:Int,
                  3072:Int,
                  3:Int,
                  3072:Int,
             
                  48:Int //channel*kernel
                )
             println("layer2 mxm") 
            
                blocked_mxm_op(
                  0: Int,
                  3072: Int,
                  144: Int,
                  64: Int,
                  0: Int,
                  0: Int,
                  196: Int,
                  144: Int,
                  0: Int,
                  144: Int,
                  64: Int,
                  196: Int,
                  144: Int
                )
                /*
                Foreach(64 by 1, 194 by 1) { (i,j) =>
                  if(j==193){
                    print(SRAM_act_mem(144+i,j))
                    println("")
                  }
                  else {
                    print(SRAM_act_mem(144+i,j) + ",")
                  }
                }
	        */
                col2im_b3(
                  0: Int,
                  144: Int,
                  0: Int,
                  208: Int,
                  196: Int,
                  64: Int,
                  14: Int
                )

                relu2D(
                  0: Int,
                  208: Int,
                  0: Int,
                  208: Int,
                  14: Int,
                  896: Int, 
                ) 
/*
                println("------Relu layer2--------")
                Foreach(0 until 896 by 1, 0 until 14 by 1){ (i,j) =>
                  println("Relu result ******* (" + (208+i) + "," + j + ") --> " + SRAM_act_mem(208+i,j))
                }*/
                maxpool2D_k2(
                  0: Int,
                  208: Int,
                  0: Int,
                  0: Int,
                  14: Int,
                  896: Int, 
                ) 
/*
                println("------MAX Pooling--------")
                Foreach(0 until 448 by 1, 0 until 7 by 1){ (i,j) =>
                  println("max pooling result ******* (" + i + "," + j + ") --> " + SRAM_act_mem(i,j))
                }*/

                /* 
                Foreach(0 until 28 by 1, 0 until 28 by 1){ (i,j) =>
                  if(i==0) {
                    println(" ^^^^^^^^^^^ col2im function call ^^^^^^^^^^  ")
                  }
                  println("read from ******* (" + j + "," + i + ") --> " + SRAM_act_mem(16+j,i))
                }
                */
                flatten(
                  0: Int,
                  0: Int,
                  7: Int,
                  448: Int,
                  7: Int,
                  448: Int
                )
                //lin1
                blocked_mxm_op(
                  6: Int,
                  0: Int,
                  3136: Int,
                  784: Int,
                  7: Int,
                  448: Int,
                  1: Int,
                  3136: Int,
                  0: Int,
                  0: Int,
                  784: Int,
                  1: Int,
                  3136: Int
                )
//lin2
                blocked_mxm_op(
                  3142: Int,
                  0: Int,
                  784: Int,
                  10: Int,
                  0: Int,
                  0: Int,
                  1: Int,
                  784: Int,
                  1: Int, //column 1
                  0: Int, //start from row 0
                  10: Int,
                  1: Int,
                  784: Int
                )
		 generic_sram_dram_copy(
                SRAM_act_mem: SRAM2[T],
                DRAM_input_mem: DRAM2[T],
                1: Int, //src
                0: Int, //src
                0: Int, //dst
                0: Int, //dst
                1: Int,
                10: Int
            ) 

		/*transpose(
		  6: Int,
		  0: Int,
		  6: Int,
		  784: Int,
		  3136: Int,
		  784: Int,
		  )*/
                
                /*println("------Flatten function--------")
                Foreach(0 until 3136 by 1){ (i) =>
                  if(i==3135){
                    println(SRAM_act_mem(0,i+7))
                  }
                  else {
                    println(SRAM_act_mem(0,i+7) + ",")
                  }

                  
                }*/



          }
		
            ///////////////////////////////////////////////
            ///////////////////////////////////////////////
            ///////////////////////////////////////////////
            ///////////////////////////////////////////////


            forward()
        }

        //Copy the DRAMs back
	val result = getMatrix(C)
	println(r"result cksum: ${result.map(a => a).reduce{_+_}}")
        val HOST_output_mem = getMatrix(DRAM_output_mem)
        //println("C (0,0) ======= " + C(0,0))
        // RUN TESTING CODE

        // YOUR TESTING CODE HERE

    }

}

