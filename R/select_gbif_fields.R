#' @title select_gbif_fields
#' @name select_gbif_fields
#'
#' @description Select columns in GBIF occurrence data
#'
#' @param columns 'standard' basic columns about what, when, where, and who collected, 'all' all available columns or list column names
#'
#' @details
#'
#' "standard" : indicated by **(standard)**
#'
#' or
#'
#' 'all':
#'
#'  * 'gbifID' **(standard)**
#'  * 'abstract'
#'  * 'accessRights'
#'  * 'accrualMethod'
#'  * 'accrualPeriodicity'
#'  * 'accrualPolicy'
#'  * 'alternative'
#'  * 'audience'
#'  * 'available'
#'  * 'bibliographicCitation' **(standard)**
#'  * 'conformsTo'
#'  * 'contributor'
#'  * 'coverage'
#'  * 'created'
#'  * 'creator'
#'  * 'date'
#'  * 'dateAccepted'
#'  * 'dateCopyrighted'
#'  * 'dateSubmitted'
#'  * 'description'
#'  * 'educationLevel'
#'  * 'extent'
#'  * 'format'
#'  * 'hasFormat'
#'  * 'hasPart'
#'  * 'hasVersion'
#'  * 'identifier'
#'  * 'instructionalMethod'
#'  * 'isFormatOf'
#'  * 'isPartOf'
#'  * 'isReferencedBy'
#'  * 'isReplacedBy'
#'  * 'isRequiredBy'
#'  * 'isVersionOf'
#'  * 'issued'
#'  * 'language' **(standard)**
#'  * 'license'
#'  * 'mediator'
#'  * 'medium'
#'  * 'modified'
#'  * 'provenance'
#'  * 'publisher'
#'  * 'references'
#'  * 'relation'
#'  * 'replaces'
#'  * 'requires'
#'  * 'rights'
#'  * 'rightsHolder'
#'  * 'source'
#'  * 'spatial'
#'  * 'subject'
#'  * 'tableOfContents'
#'  * 'temporal'
#'  * 'title'
#'  * 'type'
#'  * 'valid'
#'  * 'institutionID'
#'  * 'collectionID'
#'  * 'datasetID'
#'  * 'institutionCode' **(standard)**
#'  * 'collectionCode' **(standard)**
#'  * 'datasetName' **(standard)**
#'  * 'ownerInstitutionCode'
#'  * 'basisOfRecord' **(standard)**
#'  * 'informationWithheld' **(standard)**
#'  * 'dataGeneralizations' **(standard)**
#'  * 'dynamicProperties'
#'  * 'occurrenceID' **(standard)** # occ_search(occurrenceId='BRA:UNEMAT:HPAN:6089')
#'  * 'catalogNumber' **(standard)**
#'  * 'recordNumber' **(standard)**
#'  * 'recordedBy' **(standard)**
#'  * 'recordedByID'
#'  * 'individualCount'
#'  * 'organismQuantity'
#'  * 'organismQuantityType'
#'  * 'sex'
#'  * 'lifeStage'
#'  * 'reproductiveCondition'
#'  * 'behavior'
#'  * 'establishmentMeans'
#'  * 'degreeOfEstablishment'
#'  * 'pathway'
#'  * 'georeferenceVerificationStatus' **(standard)**
#'  * 'occurrenceStatus' **(standard)**
#'  * 'preparations'
#'  * 'disposition'
#'  * 'associatedOccurrences'
#'  * 'associatedReferences'
#'  * 'associatedSequences'
#'  * 'associatedTaxa'
#'  * 'otherCatalogNumbers'
#'  * 'occurrenceRemarks'
#'  * 'organismID'
#'  * 'organismName'
#'  * 'organismScope'
#'  * 'associatedOrganisms'
#'  * 'previousIdentifications'
#'  * 'organismRemarks'
#'  * 'materialSampleID'
#'  * 'eventID'
#'  * 'parentEventID'
#'  * 'fieldNumber'
#'  * 'eventDate' **(standard)**
#'  * 'eventTime'
#'  * 'startDayOfYear'
#'  * 'endDayOfYear'
#'  * 'year' **(standard)**
#'  * 'month' **(standard)**
#'  * 'day' **(standard)**
#'  * 'verbatimEventDate'
#'  * 'habitat' **(standard)**
#'  * 'samplingProtocol'
#'  * 'sampleSizeValue'
#'  * 'sampleSizeUnit'
#'  * 'samplingEffort'
#'  * 'fieldNotes' **(standard)**
#'  * 'eventRemarks' **(standard)**
#'  * 'locationID'   **(standard)**
#'  * 'higherGeographyID'
#'  * 'higherGeography' **(standard)**
#'  * 'continent'
#'  * 'waterBody'
#'  * 'islandGroup' **(standard)**
#'  * 'island' **(standard)**
#'  * 'countryCode' **(standard)**
#'  * 'stateProvince' **(standard)**
#'  * 'county' **(standard)**
#'  * 'municipality' **(standard)**
#'  * 'locality' **(standard)**
#'  * 'verbatimLocality' **(standard)**
#'  * 'verbatimElevation'
#'  * 'verticalDatum'
#'  * 'verbatimDepth'
#'  * 'minimumDistanceAboveSurfaceInMeters'
#'  * 'maximumDistanceAboveSurfaceInMeters'
#'  * 'locationAccordingTo'
#'  * 'locationRemarks' **(standard)**
#'  * 'decimalLatitude' **(standard)**
#'  * 'decimalLongitude' **(standard)**
#'  * 'coordinateUncertaintyInMeters'
#'  * 'coordinatePrecision'
#'  * 'pointRadiusSpatialFit'
#'  * 'verbatimCoordinateSystem' **(standard)**
#'  * 'verbatimSRS'
#'  * 'footprintWKT'
#'  * 'footprintSRS'
#'  * 'footprintSpatialFit'
#'  * 'georeferencedBy'
#'  * 'georeferencedDate'
#'  * 'georeferenceProtocol'
#'  * 'georeferenceSources'
#'  * 'georeferenceRemarks'
#'  * 'geologicalContextID'
#'  * 'earliestEonOrLowestEonothem'
#'  * 'latestEonOrHighestEonothem'
#'  * 'earliestEraOrLowestErathem'
#'  * 'latestEraOrHighestErathem'
#'  * 'earliestPeriodOrLowestSystem'
#'  * 'latestPeriodOrHighestSystem'
#'  * 'earliestEpochOrLowestSeries'
#'  * 'latestEpochOrHighestSeries'
#'  * 'earliestAgeOrLowestStage'
#'  * 'latestAgeOrHighestStage'
#'  * 'lowestBiostratigraphicZone'
#'  * 'highestBiostratigraphicZone'
#'  * 'lithostratigraphicTerms'
#'  * 'group'
#'  * 'formation'
#'  * 'member'
#'  * 'bed'
#'  * 'identificationID'
#'  * 'verbatimIdentification' **(standard)**
#'  * 'identificationQualifier' **(standard)**
#'  * 'typeStatus' **(standard)**
#'  * 'identifiedBy' **(standard)**
#'  * 'identifiedByID'
#'  * 'dateIdentified' **(standard)**
#'  * 'identificationReferences'
#'  * 'identificationVerificationStatus'
#'  * 'identificationRemarks'
#'  * 'taxonID'
#'  * 'scientificNameID'
#'  * 'acceptedNameUsageID'
#'  * 'parentNameUsageID'
#'  * 'originalNameUsageID'
#'  * 'nameAccordingToID'
#'  * 'namePublishedInID'
#'  * 'taxonConceptID'
#'  * 'scientificName' **(standard)**
#'  * 'acceptedNameUsage'
#'  * 'parentNameUsage'
#'  * 'originalNameUsage'
#'  * 'nameAccordingTo'
#'  * 'namePublishedIn'
#'  * 'namePublishedInYear'
#'  * 'higherClassification'
#'  * 'kingdom'
#'  * 'phylum'
#'  * 'class'
#'  * 'order'
#'  * 'family' **(standard)**
#'  * 'subfamily'
#'  * 'genus'
#'  * 'genericName'
#'  * 'subgenus'
#'  * 'infragenericEpithet'
#'  * 'specificEpithet'
#'  * 'infraspecificEpithet'
#'  * 'cultivarEpithet'
#'  * 'taxonRank' **(standard)**
#'  * 'verbatimTaxonRank'
#'  * 'vernacularName'
#'  * 'nomenclaturalCode' **(standard)**
#'  * 'taxonomicStatus' **(standard)**
#'  * 'nomenclaturalStatus'
#'  * 'taxonRemarks'
#'  * 'datasetKey'
#'  * 'publishingCountry'
#'  * 'lastInterpreted'
#'  * 'elevation'
#'  * 'elevationAccuracy'
#'  * 'depth'
#'  * 'depthAccuracy'
#'  * 'distanceAboveSurface'
#'  * 'distanceAboveSurfaceAccuracy'
#'  * 'issue' **(standard)**
#'  * 'mediaType' **(standard)**
#'  * 'hasCoordinate' **(standard)**
#'  * 'hasGeospatialIssues' **(standard)**
#'  * 'taxonKey'
#'  * 'acceptedTaxonKey'
#'  * 'kingdomKey'
#'  * 'phylumKey'
#'  * 'classKey'
#'  * 'orderKey'
#'  * 'familyKey'
#'  * 'genusKey'
#'  * 'subgenusKey'
#'  * 'speciesKey'
#'  * 'species'
#'  * 'acceptedScientificName'
#'  * 'verbatimScientificName' **(standard)**
#'  * 'typifiedName'
#'  * 'protocol'
#'  * 'lastParsed'
#'  * 'lastCrawled'
#'  * 'repatriated'
#'  * 'relativeOrganismQuantity'
#'  * 'level0Gid'
#'  * 'level0Name' **(standard)**
#'  * 'level1Gid'
#'  * 'level1Name' **(standard)**
#'  * 'level2Gid'
#'  * 'level2Name' **(standard)**
#'  * 'level3Gid'
#'  * 'level3Name' **(standard)**
#'  * 'iucnRedListCategory'
#'
#' @return
#' Character vector of selected column names
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[ParsGBIF]{extract_gbif_issue}}, \code{\link[ParsGBIF]{prepare_gbif_occurrence_data}}
#'
#' @examples
#' \donttest{
#' # Get standard columns
#' col_sel <- select_gbif_fields(columns = 'standard')
#'
#' # Get all columns
#' col_sel <- select_gbif_fields(columns = 'all')
#'
#' # Get specific columns
#' col_sel <- select_gbif_fields(columns = c('gbifID', 'scientificName', 'decimalLatitude'))
#' }
#'
#' @export
select_gbif_fields <- function(columns = 'standard')
{
  col_all <- c(
    'gbifID',
    'abstract',
    'accessRights',
    'accrualMethod',
    'accrualPeriodicity',
    'accrualPolicy',
    'alternative',
    'audience',
    'available',
    'bibliographicCitation',
    'conformsTo',
    'contributor',
    'coverage',
    'created',
    'creator',
    'date',
    'dateAccepted',
    'dateCopyrighted',
    'dateSubmitted',
    'description',
    'educationLevel',
    'extent',
    'format',
    'hasFormat',
    'hasPart',
    'hasVersion',
    'identifier',
    'instructionalMethod',
    'isFormatOf',
    'isPartOf',
    'isReferencedBy',
    'isReplacedBy',
    'isRequiredBy',
    'isVersionOf',
    'issued',
    'language',
    'license',
    'mediator',
    'medium',
    'modified',
    'provenance',
    'publisher',
    'references',
    'relation',
    'replaces',
    'requires',
    'rights',
    'rightsHolder',
    'source',
    'spatial',
    'subject',
    'tableOfContents',
    'temporal',
    'title',
    'type',
    'valid',
    'institutionID',
    'collectionID',
    'datasetID',
    'institutionCode',
    'collectionCode',
    'datasetName',
    'ownerInstitutionCode',
    'basisOfRecord',
    'informationWithheld',
    'dataGeneralizations',
    'dynamicProperties',
    'occurrenceID',
    'catalogNumber',
    'recordNumber',
    'recordedBy',
    'recordedByID',
    'individualCount',
    'organismQuantity',
    'organismQuantityType',
    'sex',
    'lifeStage',
    'reproductiveCondition',
    'behavior',
    'establishmentMeans',
    'degreeOfEstablishment',
    'pathway',
    'georeferenceVerificationStatus',
    'occurrenceStatus',
    'preparations',
    'disposition',
    'associatedOccurrences',
    'associatedReferences',
    'associatedSequences',
    'associatedTaxa',
    'otherCatalogNumbers',
    'occurrenceRemarks',
    'organismID',
    'organismName',
    'organismScope',
    'associatedOrganisms',
    'previousIdentifications',
    'organismRemarks',
    'materialSampleID',
    'eventID',
    'parentEventID',
    'fieldNumber',
    'eventDate',
    'eventTime',
    'startDayOfYear',
    'endDayOfYear',
    'year',
    'month',
    'day',
    'verbatimEventDate',
    'habitat',
    'samplingProtocol',
    'sampleSizeValue',
    'sampleSizeUnit',
    'samplingEffort',
    'fieldNotes',
    'eventRemarks',
    'locationID',
    'higherGeographyID',
    'higherGeography',
    'continent',
    'waterBody',
    'islandGroup',
    'island',
    'countryCode',
    'stateProvince',
    'county',
    'municipality',
    'locality',
    'verbatimLocality',
    'verbatimElevation',
    'verticalDatum',
    'verbatimDepth',
    'minimumDistanceAboveSurfaceInMeters',
    'maximumDistanceAboveSurfaceInMeters',
    'locationAccordingTo',
    'locationRemarks',
    'decimalLatitude',
    'decimalLongitude',
    'coordinateUncertaintyInMeters',
    'coordinatePrecision',
    'pointRadiusSpatialFit',
    'verbatimCoordinateSystem',
    'verbatimSRS',
    'footprintWKT',
    'footprintSRS',
    'footprintSpatialFit',
    'georeferencedBy',
    'georeferencedDate',
    'georeferenceProtocol',
    'georeferenceSources',
    'georeferenceRemarks',
    'geologicalContextID',
    'earliestEonOrLowestEonothem',
    'latestEonOrHighestEonothem',
    'earliestEraOrLowestErathem',
    'latestEraOrHighestErathem',
    'earliestPeriodOrLowestSystem',
    'latestPeriodOrHighestSystem',
    'earliestEpochOrLowestSeries',
    'latestEpochOrHighestSeries',
    'earliestAgeOrLowestStage',
    'latestAgeOrHighestStage',
    'lowestBiostratigraphicZone',
    'highestBiostratigraphicZone',
    'lithostratigraphicTerms',
    'group',
    'formation',
    'member',
    'bed',
    'identificationID',
    'verbatimIdentification',
    'identificationQualifier',
    'typeStatus',
    'identifiedBy',
    'identifiedByID',
    'dateIdentified',
    'identificationReferences',
    'identificationVerificationStatus',
    'identificationRemarks',
    'taxonID',
    'scientificNameID',
    'acceptedNameUsageID',
    'parentNameUsageID',
    'originalNameUsageID',
    'nameAccordingToID',
    'namePublishedInID',
    'taxonConceptID',
    'scientificName',
    'acceptedNameUsage',
    'parentNameUsage',
    'originalNameUsage',
    'nameAccordingTo',
    'namePublishedIn',
    'namePublishedInYear',
    'higherClassification',
    'kingdom',
    'phylum',
    'class',
    'order',
    'family',
    'subfamily',
    'genus',
    'genericName',
    'subgenus',
    'infragenericEpithet',
    'specificEpithet',
    'infraspecificEpithet',
    'cultivarEpithet',
    'taxonRank',
    'verbatimTaxonRank',
    'vernacularName',
    'nomenclaturalCode',
    'taxonomicStatus',
    'nomenclaturalStatus',
    'taxonRemarks',
    'datasetKey',
    'publishingCountry',
    'lastInterpreted',
    'elevation',
    'elevationAccuracy',
    'depth',
    'depthAccuracy',
    'distanceAboveSurface',
    'distanceAboveSurfaceAccuracy',
    'issue',
    'mediaType',
    'hasCoordinate',
    'hasGeospatialIssues',
    'taxonKey',
    'acceptedTaxonKey',
    'kingdomKey',
    'phylumKey',
    'classKey',
    'orderKey',
    'familyKey',
    'genusKey',
    'subgenusKey',
    'speciesKey',
    'species',
    'acceptedScientificName',
    'verbatimScientificName',
    'typifiedName',
    'protocol',
    'lastParsed',
    'lastCrawled',
    'repatriated',
    'relativeOrganismQuantity',
    'level0Gid',
    'level0Name',
    'level1Gid',
    'level1Name',
    'level2Gid',
    'level2Name',
    'level3Gid',
    'level3Name',
    'iucnRedListCategory'
  )

  if(!columns %in% c('standard','all'))
  {
    if (!columns %in% col_all )
    {
      stop("Unknown field in GBIF!")
    }
  }

  if (length(columns)==0)
  {
    stop("Inform a field, standard or all")
  }

  if (length(columns)>0)
  {
    col_sel <- columns
  }

  if (columns == 'standard')
  {
    col_sel <- c(
      'gbifID',
      # abstract
      # accessRights
      # accrualMethod
      # accrualPeriodicity
      # accrualPolicy
      # alternative
      # audience
      # available
      'bibliographicCitation',
      # conformsTo
      # contributor
      # coverage
      # created
      # creator
      # date
      # dateAccepted
      # dateCopyrighted
      # dateSubmitted
      # description
      # educationLevel
      # extent
      # format
      # hasFormat
      # hasPart
      # hasVersion
      # identifier
      # instructionalMethod
      # isFormatOf
      # isPartOf
      # isReferencedBy
      # isReplacedBy
      # isRequiredBy
      # isVersionOf
      # issued
      'language',
      # license
      # mediator
      # medium
      # modified
      # provenance
      # publisher
      # references
      # relation
      # replaces
      # requires
      # rights
      # rightsHolder
      # source
      # spatial
      # subject
      # tableOfContents
      # temporal
      # title
      # type
      # valid
      # institutionID
      # collectionID
      # datasetID
      'institutionCode',
      'collectionCode',
      'datasetName',
      # ownerInstitutionCode
      'basisOfRecord',
      'informationWithheld', # especifica geo referenciamento
      'dataGeneralizations', # informações de campo
      # 'dynamicProperties', # DNA voucher
      'occurrenceID', # occ_search(occurrenceId='BRA:UNEMAT:HPAN:6089')
      'catalogNumber',
      'recordNumber',
      'recordedBy',
      # recordedByID
      # individualCount
      # organismQuantity
      # organismQuantityType
      # sex
      # lifeStage
      # reproductiveCondition
      # behavior
      # establishmentMeans
      # degreeOfEstablishment
      # pathway
      'georeferenceVerificationStatus',
      'occurrenceStatus',
      # preparations
      # disposition
      # associatedOccurrences
      # associatedReferences
      # associatedSequences
      # associatedTaxa
      # otherCatalogNumbers
      # occurrenceRemarks
      # organismID
      # organismName
      # organismScope
      # associatedOrganisms
      # previousIdentifications
      # organismRemarks
      # materialSampleID
      # eventID
      # parentEventID
      # fieldNumber
      'eventDate',
      # eventTime
      # startDayOfYear
      # endDayOfYear
      'year',
      'month',
      'day',
      # verbatimEventDate
      'habitat',
      # samplingProtocol
      # sampleSizeValue
      # sampleSizeUnit
      # samplingEffort
      'fieldNotes',
      'eventRemarks',
      'locationID',
      # higherGeographyID
      'higherGeography',
      # continent
      # waterBody
      'islandGroup',
      'island',
      'countryCode',
      'stateProvince',
      'county',
      'municipality',
      'locality',
      'verbatimLocality',
      # verbatimElevation
      # verticalDatum
      # verbatimDepth
      # minimumDistanceAboveSurfaceInMeters
      # maximumDistanceAboveSurfaceInMeters
      # locationAccordingTo
      'locationRemarks', # when countryCode is empty
      'decimalLatitude',
      'decimalLongitude',

      # 'coordinateUncertaintyInMeters', # only if georeferencedDate no empty

      # coordinatePrecision
      # pointRadiusSpatialFit
      'verbatimCoordinateSystem', # dicas sobre sistema geografico
      # verbatimSRS
      # footprintWKT
      # footprintSRS
      # footprintSpatialFit
      # georeferencedBy
      # 'georeferencedDate' # only if coordinateUncertaintyInMeters no empty
      # georeferenceProtocol
      # georeferenceSources
      # georeferenceRemarks
      # geologicalContextID
      # earliestEonOrLowestEonothem
      # latestEonOrHighestEonothem
      # earliestEraOrLowestErathem
      # latestEraOrHighestErathem
      # earliestPeriodOrLowestSystem
      # latestPeriodOrHighestSystem
      # earliestEpochOrLowestSeries
      # latestEpochOrHighestSeries
      # earliestAgeOrLowestStage
      # latestAgeOrHighestStage
      # lowestBiostratigraphicZone
      # highestBiostratigraphicZone
      # lithostratigraphicTerms
      # group
      # formation
      # member
      # bed
      # identificationID
      'verbatimIdentification',
      'identificationQualifier',
      'typeStatus',
      'identifiedBy',
      # identifiedByID
      'dateIdentified',
      # identificationReferences
      # identificationVerificationStatus
      # identificationRemarks
      # taxonID
      # scientificNameID
      # acceptedNameUsageID
      # parentNameUsageID
      # originalNameUsageID
      # nameAccordingToID
      # namePublishedInID
      # taxonConceptID
      'scientificName',
      # acceptedNameUsage
      # parentNameUsage
      # originalNameUsage
      # nameAccordingTo
      # namePublishedIn
      # namePublishedInYear
      # higherClassification
      # kingdom
      # phylum
      # class
      # order
      'family',
      # subfamily
      # genus
      # genericName
      # subgenus
      # infragenericEpithet
      # specificEpithet
      # infraspecificEpithet
      # cultivarEpithet
      'taxonRank',
      # verbatimTaxonRank
      # vernacularName
      'nomenclaturalCode',
      'taxonomicStatus',
      # nomenclaturalStatus
      # taxonRemarks
      # datasetKey
      # publishingCountry
      # lastInterpreted
      # elevation
      # elevationAccuracy
      # depth
      # depthAccuracy
      # distanceAboveSurface
      # distanceAboveSurfaceAccuracy
      'issue',
      'mediaType',
      'hasCoordinate',
      'hasGeospatialIssues',
      # taxonKey
      # acceptedTaxonKey
      # kingdomKey
      # phylumKey
      # classKey
      # orderKey
      # familyKey
      # genusKey
      # subgenusKey
      # speciesKey
      # species
      # acceptedScientificName
      'verbatimScientificName',
      # typifiedName
      # protocol
      # lastParsed
      # lastCrawled
      # repatriated
      # relativeOrganismQuantity
      # 'level0Gid',
      'level0Name',
      # level1Gid
      'level1Name',
      # level2Gid
      'level2Name',
      # level3Gid
      'level3Name'
      # iucnRedListCategory
    )
  }

  if (columns == 'all')
  {
    col_sel <-col_all
  }


  return(col_sel)

}

# excluir Ctrl_informationWithheld	Ctrl_dataGeneralizations Ctrl_georeferenceVerificationStatus
# excluir Ctrl_islandGroup	Ctrl_island Ctrl_verbatimCoordinateSystem	Ctrl_verbatimIdentification
# excluir Ctrl_verbatimLocality Ctrl_locationRemarks Ctrl_verbatimCoordinateSystem	Ctrl_verbatimIdentification

