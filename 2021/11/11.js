var g1, g2;


const fs = require('fs');
try {
    const data= fs.readFileSync('given.txt', 'utf8')
    g1 = data;
    // g2 = data2;
} catch (err) {
    console.error(err)
}



// Create matrix
function genMatrix (g){
    var matrix = [], newStr = "";

    for( var i = 0; i < g.length; i++ ) 
        if( !g[i].includes('\r') )
            newStr += g[i];



    matrix = newStr.split("\n")

    for (var i = 0; i < matrix.length; i++){
            matrix[i] = parseInt(matrix[i]);
    }
    
    

    return matrix;
}

// Using standard input, change integers to elements in ij array
function makeMatrix (matrix){
    var ijMatrix = [], y, z;

    for(var i = 0; i < matrix.length; i++){
        ijMatrix[i] = [];
        y = matrix[i];
        z = String(y).split("").map((y)=> { return Number(y) });

        for(var j = 0; j < z.length; j++){
            ijMatrix[i].push(z[j])
        } 
    }
    return ijMatrix;
}


// First step, increment each element by 1
function incrementM (matrix, trackFlash, fCount){
    for (var i = 0; i < matrix.length; i++){
        for(var j = 0; j < matrix.length; j++){

            if(matrix[i][j] > 9) {
                matrix[i][j] = 0;
                continue;
            }

            matrix[i][j] +=1;
            if(matrix[i][j] > 9) {
                trackFlash.push([i,j]);
                fCount["cnt"] += 1;
            }
        }
    }
    //console.log("Track Flash (x > 9): ", trackFlash)
    return trackFlash.length;
}

function finalReduction(matrix){
    for (var i = 0; i < matrix.length; i++){
        for(var j = 0; j < matrix.length; j++){
            if(matrix[i][j] > 9) 
                matrix[i][j] = 0;
        }
    }
}

// Get neighbors to flash
function neighbors (coords, n) {

    
    if(coords.length == 0) return;

    //console.log("Coordinates for a Flash: ", coords)

    var r = coords[0][0], s = coords[0][1], x, y; 
    adjacent = [
        [[], [], []], 
        [[], [], []], 
        [[], [], []]];

    x = r - 1;

    for(var i = 0; i < 3; i++){
        y = s - 1;

        for(var j = 0; j < 3; j++){
            if(x < 0 || y < 0) adjacent[i][j] = -1;
            else if (x >= n || y >= n) adjacent[i][j] = -1;
            else {
                adjacent[i][j] = [x, y]
            }

            y+=1;
        }
        x+=1;
    }

    //console.log("Valid neighbors: ", adjacent);
    return adjacent;
}


function flash(matrix, trackFlash, neighbors, fCount){
    //console.log("\n\n\n");

    if(trackFlash.length == 0) return;

    //log("Flash Neighbors:    ", neighbors)

    var x, y;

    // Increment flash neighbors
    for(var i = 0; i < 3; i++){

        for(var j = 0; j < 3; j++){

            if(neighbors[i][j] == -1) {
                continue;
            }

            x = neighbors[i][j][0];
            y = neighbors[i][j][1];

            //console.log("For:                 ", neighbors[i][j][0], "              " , neighbors[i][j][1], "            ", matrix[x][y], "           ", neighbors[i], "  " , trackFlash) 


            matrix[x][y]+=1;
            //console.log("For: ", matrix, "                ", neighbors[i][j][0], "              " , neighbors[i][j][1], "            ", matrix[x][y], "           ", neighbors[i],"\n",) 

            if(matrix[x][y] === 10){
                trackFlash.push([neighbors[i][j][0], neighbors[i][j][1]]);
                fCount["cnt"] += 1;
                //console.log(fCount)
            }


        }
    }

    //console.log(matrix)

    return;
};

function step (matrix){
    var trackFlash = [], nBors, stepN = 5000000;

    // Track Flashes
    var flashCount = {
        cnt : 0
    };  

    //console.log("Input: ", matrix);

 
    for(var n = 0; n < stepN; n++){
        flashNodes = incrementM(matrix, trackFlash, flashCount);

        //console.log("---------------------------------------------------------------------Flash Nodes: " + flashNodes, matrix)

        while(flashNodes > 0) {
            // console.log("TrackFlash, Length: ",trackFlash, matrix.length, "           Flash nodes: ", flashNodes)
            // console.log(matrix)
            nBors = neighbors(trackFlash, matrix.length);
            flash(matrix, trackFlash, nBors, flashCount);
            trackFlash.shift();         
            flashNodes = trackFlash.length;  
        }



        trackFlash = [];

        // for(var i = 0; i < flashNodes; i++){
            
        //     console.log("TrackFlash, Length: ",trackFlash, matrix.length)
        //     console.log(matrix)
        //     nBors = neighbors(trackFlash, matrix.length);
        //     flash(matrix, trackFlash, nBors);
        //     trackFlash.shift();

        // }
        finalReduction(matrix);

    }


    //console.log("Result: ", matrix)
    return flashCount["cnt"]
}


// var testInc1 = [
//     [5,2,0],
//     [4,3,6],
//     [6,7,9]
// ]
// step(testInc1)

// var testInc2 = [
//     [9,1,1,1],
//     [1,1,1,1],
//     [1,1,1,1],
//     [1,1,1,9]
// ]
//step(testInc2)

// step(makeMatrix(genMatrix(g1)))
// console.log(makeMatrix(genMatrix(g1)))


// var testInc3 = [
//     [1,2,3,4],
//     [5,6,9,8],
//     [9,2,2,3],
//     [1,1,6,5]]

// step(testInc3 )

// var testInc4 = [
//     [1,1,1,1],
//     [2,2,2,2],
//     [3,3,3,3],
//     [0,0,0,0]]
// step(testInc4)

// var y = x[0]

// var z = String(y).split("").map((y)=>{
//     return Number(y)
//   })
// console.log(x)
// console.log(y)
// console.log(z)

// let adv1 = [
// [1,1,1,1,1],
// [1,9,9,9,1],
// [1,9,1,9,1],
// [1,9,9,9,1],
// [1,1,1,1,1]
// ]

// step(adv1)

console.time("Runtime")
console.log(step(makeMatrix(genMatrix(g2))));
console.timeEnd("Runtime")

// let s2 = [
// [8,8,0,7,4,7,6,5,5,5],
// [5,0,8,9,0,8,7,0,5,4],
// [8,5,9,7,8,8,9,6,0,8],
// [8,4,8,5,7,6,9,6,0,0],
// [8,7,0,0,9,0,8,8,0,0],
// [6,6,0,0,0,8,8,9,8,9],
// [6,8,0,0,0,0,5,9,4,3],
// [0,0,0,0,0,0,7,4,5,6],
// [9,0,0,0,0,0,0,8,7,6],
// [8,7,0,0,0,0,6,8,4,8]
// ]

// step(s2)


// let testing = [
//     [8,9,7,2,3,4],
//     [2,7,9,0,0,0],
//     [0,7,6,9,9,5],
//     [0,9,1,7,5,9],
//     [0,1,1,1,9,9],
//     [0,0,0,0,0,0]]
// step(testing)