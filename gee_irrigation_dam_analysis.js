
///////get dams and reservoirs
var gdat_dams = ee.FeatureCollection("projects/sat-io/open-datasets/GDAT/GDAT_V1_DAMS");
var dams=gdat_dams.filter(ee.Filter.eq('Main_P','Irrigation')).filter(ee.Filter.neq('Height','')) //Only keep main purpos is irrigation
          .map(function(x){return(x.set('Height',ee.Number.parse(x.get('Height'))))})
          .filter(ee.Filter.gt('Height',15))//only keep dams higher than 15 meters ICOLD
          .filter(ee.Filter.inList('feature_ID', dams0.aggregate_array('Feature_ID')).not()).set('PMdata',0) //Replace with piyush corrected coords
          .map(function(feature) {return feature.set('geometry_type', feature.geometry() ? feature.geometry().type() : 'null')}) //only keep sngle point geom
          .filter(ee.Filter.eq('geometry_type', 'Point'))
          .merge(dams0.set('PMdata',1))
//that0s 5110 dams  
//print(dams.size())

var dams=dams0

var Lakes=ResPM
            .filter(ee.Filter.eq('validLake',true))
            .select(['Feature_ID','area','distance','ResElev','ResCapBathy'])
            .map(function(ft){return(ft.set('Height',ee.Feature(dams.filter(ee.Filter.eq('Feature_ID',ft.get('Feature_ID'))).first()).get('Height')))})

var dams = dams.filter(ee.Filter.inList('Feature_ID', Lakes.aggregate_array('Feature_ID')));


///////Params
var dz=50 //Elevation above crest to define CA
var resol=30



///////DATA PREPROCESSING
//Mosaic'd DEM
demx=demx.mean() 

//Cropland suitability proxied as cropland landcover in 2019
var lc=ee.Image(lc0.filter(ee.Filter.eq('system:index','2019')).first()).select(0)
var crp=lc.eq(40)//.or(lc.eq(20)).or(lc.eq(30)).or(lc.eq(40))

//Ratio between reservoir capacity and reaservoir area x dam height (for reservoirs with bathymetry)
var ResPMCap=Lakes.filter(ee.Filter.gt('ResCapBathy',0))
var Area=ee.Number.parse(ResPMCap.aggregate_array('area').reduce(ee.Reducer.mean()));
var Cap=ee.Number.parse(ResPMCap.aggregate_array('ResCapBathy').reduce(ee.Reducer.mean()));
var Height=ee.Number.parse(dams.filter(ee.Filter.inList('Feature_ID', ResPMCap.aggregate_array('Feature_ID').distinct())).aggregate_array('Height').reduce(ee.Reducer.mean()));
var BackupCapRatio=Cap.divide(Height.multiply(Area))

//Debugging
//dams=dams.randomColumn('rd',123).sort('rd').limit(5)
var dam=ee.Feature(dams.filter(ee.Filter.eq('system:index','00000000000000000180')).first())
//////////////////////////////////////////////////////////////////////
var getCA = function(dam) {
  var sanitizeValue = function(value) {
    return ee.Algorithms.If(
      ee.Algorithms.IsEqual(value, ""), // Check if the value is an empty string
      0,                       // Replace empty string with 0
      ee.Number.parse(value).max(0)  // Convert null to 0 or keep the original number
    );
  };

  //fetch coutnry-level yields
  var country = ee.Feature(Country.filterBounds(dam.geometry()).first()).get('ADM0_CODE')
  var yieldFeature = Yields.filter(ee.Filter.eq('gaul', country)).first();
  var yieldValue = ee.Algorithms.If(yieldFeature, yieldFeature.get('Yield'), null); // Handle missing values
  
  //Fetch reservoir
  var res0=ee.Feature(Lakes.filter(ee.Filter.eq('Feature_ID',dam.get('Feature_ID'))).first())

  // --- canals linked to this reservoir (within 110 m of reservoir polygon) ---
  var canalNear = Canals.filterBounds(res0.geometry().buffer(110));
  
  var canalFIDs = ee.List(canalNear.aggregate_array('Feature_ID')).distinct();
  var hasCanal = canalFIDs.length().gt(0);
  
  // all canals with the same Feature_ID(s) (may include canals far away but same network ID)
  var canalsSel = ee.FeatureCollection(
    ee.Algorithms.If(
      hasCanal,
      Canals.filter(ee.Filter.inList('Feature_ID', canalFIDs)),
      ee.FeatureCollection([])
    )
  );


  //take largest reported reservoir capacity from GDAT
  var attr1 = ee.Number(sanitizeValue(dam.get('Volume_Con'))).multiply(1e6);
  var attr2 = ee.Number(sanitizeValue(dam.get('Volume_Max'))).multiply(1e6)
  var attr3 = ee.Number(sanitizeValue(dam.get('Volume_Min'))).multiply(1e6)
  var attr4 = ee.Number(sanitizeValue(dam.get('Volume_Rep'))).multiply(1e6)
  var attr5 = ee.Number(sanitizeValue(dam.get('Volume_Rep'))).multiply(1e6)
  var ResCapGDAT = attr1.max(attr2).max(attr3).max(attr4).max(attr5);
  
  //get Bathymetry-estimated capacity
  var ResCapBathy = ee.Number(res0.get('ResCapBathy'))

  //Make estimate of capacity based on dam height and lake area (using the proportionality factor form available bathymetry)
  var backupCap=ee.Number(sanitizeValue(dam.get('Height'))).divide(BackupCapRatio).multiply(ee.Number.parse(res0.get('area')))

 
  ///Select final reservoir capacity: 1 take bathymetry if available. 2 take hydrodat estimate, 3 make own estimate
  /*
  var ResCap = ee.Algorithms.If(
      ResCapBathy.gt(0), // Condition: ResCapDEM is not -1
      ResCapBathy,         // Take ResCapDEM if available
      ee.Algorithms.If(
        ResCapBathy.gt(0),      // Check if ResCapGDAT exists
        ResCapGDAT,      // Take ResCapGDAT if available
      backupCap        // Otherwise, use backupCap
     )
    );
  
  
  */
  //Mean of strictly positive estimates of rescap
 var ResCap = ee.Number(
    ee.List([ResCapBathy, ResCapGDAT, backupCap])
      .filter(ee.Filter.gt('item', 0))
      .reduce(ee.Reducer.mean())
    );
 
 
  //////////////Command area criteria
  // 1. Topography: should be below max altitude pumpable from reservoir
  var demdam=ee.Image(1).mask(demx.select('DEM').lt(ee.Number.parse(res0.get('ResElev')).add(dz)))

  // 2. Hydrology: should be within the same basin whose degree is determined by reservoir capacity
  /*
  var bas0 = ee.FeatureCollection(
      ee.Algorithms.If(
        ee.Number.parse(ResCap).lt(100e6),
        bas7,
        ee.Algorithms.If(
         ee.Number.parse(ResCap).lte(1000e6),
          bas6,
          bas5
        )
      )
    ).filterBounds(dam.geometry());
  */
  // pick basin resolution by ResCap (same logic you already have)
  var basL = ee.FeatureCollection(
    ee.Algorithms.If(
      ee.Number(ResCap).lt(100e6),
      bas7,
      ee.Algorithms.If(
        ee.Number(ResCap).lte(1000e6),
        bas6,
        bas5
      )
    )
  );
  
  // basins containing the reservoir (your original idea)
  var basRes = basL.filterBounds(dam.geometry());
  
  // basins containing selected canals (expanded domain)
  var basCan = basL.filterBounds(canalsSel.geometry());
  
  // final basin domain used for CA: reservoir basins + canal basins (if any)
  var basDomain = ee.FeatureCollection(
    ee.Algorithms.If(
      hasCanal,
      basRes.merge(basCan).distinct(['HYBAS_ID']),   // change field if your basin ID differs
      basRes
    )
  );
  // 3. Country borders: Does not cross country borders, unless the reservoir itself crosses a country border
  var Cntr = Country.filterBounds(res0.geometry());
   
  //Initial Command Area
  var CA0 = demdam
    .clipToCollection(basDomain)   // safer than clip(basDomain)
    .clip(Cntr)
    .subtract(ee.FeatureCollection([res0]).reduceToImage(['Feature_ID'], ee.Reducer.count()));

  CA0 = CA0.mask(CA0);

  //4. Reservoir yield: max distance from reservoir given soil suitability and reservoir yield
  var TheoryCA=ee.Number.parse(yieldValue)//Theoretical CA area given reservoir volume and country-average efficiency (total irrigated divided by total capacity)
          .multiply(ee.Number.parse(ResCap)) 
     
  var EstimatedCA = ee.Image.pixelArea().updateMask(CA0.multiply(crp)) //Estimated CA area assuming that hist cropland is a proxy of crop suitability
          .reduceRegion({
              reducer: ee.Reducer.sum(),
              geometry: basDomain.geometry(),
              scale: resol, // Set this to your raster's resolution
              maxPixels: 1e13
            }).get('area')
            

  var R=TheoryCA.divide(ee.Number.parse(EstimatedCA))//Ratio of Theory vs Estimated areas
                .min(1); //Captted at 1. If theory is alrger than estimated. 


  var resdist=ee.FeatureCollection([res0]).distance({searchRadius: 2000000, maxError: 100})//

  var RDist = ee.Algorithms.If(ee.Number.parse(EstimatedCA).eq(0),0, 
                resdist.mask(CA0.multiply(crp)) //distances of cropland (!!) from reservoir
                  .reduceRegion({ //Distance percentile corresponding to area ratio
                    reducer: ee.Reducer.percentile([ee.Number.parse(R).multiply(100)]),
                    geometry: basDomain.geometry(),
                    scale: resol, // Adjust to your raster's resolution
                    maxPixels: 1e13
                  }).get('distance'));
  

  ///Once we have the distance, we can threshold the command area while not worrying abount cropland,
  var CA=ee.Image(1).mask(resdist.lte(ee.Number(RDist)).multiply(CA0).multiply(crp))

  //Vectorize and add attributes
  var CAv=ee.Algorithms.If(ee.Number.parse(EstimatedCA).eq(0).or(ee.Number.parse(RDist).eq(0)),ee.Feature(dam.geometry()).buffer(1).set('validCA',false), 
    ee.Feature(CA.reduceToVectors({
      geometry:res0.buffer(RDist).geometry(),
      scale: resol,
      geometryType: 'Polygon',
      bestEffort:true,
      maxPixels:1e15,
      tileScale:4
    }).map(function(ft){return(ft.set('area',ft.geometry().area(resol)))})
    .filter(ee.Filter.gt('area',200000))
    .union(resol).first()).set('validCA',true));
    //sets 20 ha as minimum cluster size to justify canal connection

  CAv=ee.Feature(CAv)
  

  CAv=CAv.set('area',CAv.geometry().area()).set('Feature_ID',dam.get('Feature_ID')).set('MaxDist',RDist)//.set('FrcCrp',crpF)
          .set('RAreaPct',R).set('RBackupCap',BackupCapRatio).set('Geom_type',CAv.geometry().type())
          .set('backupCap',backupCap)
          .set({'dH':dz,'Yield': yieldValue, 'ResArea':res0.get('area'),'ResCapBathy':res0.get('ResCapBathy'),'ResElev':res0.get('ResElev'),'_minOccur':res0.get('_minOccur').occurrence,'_pctAreaPerm':res0.get('_pctAreaPerm').occurrence})
          .set('ResCap',ResCap).set('hasCanal', hasCanal)


 
 return(CAv)
  
}

//Map.addLayer(getCA(ee.Feature(dams.filter(ee.Filter.eq('Feature_ID',14848)).first())))

var CAs=dams
            .map(getCA)
            .filter(ee.Filter.neq('Geom_type','Empty'))
print(CAs.first())
  // Export the batch to the specified folder
Export.table.toAsset({
  collection: CAs,
  description: 'CA' + dz, // Concatenate using plain JavaScript
  assetId: 'PiyushDams/CA_withcanals_' + dz // Concatenate using plain JavaScript
});








