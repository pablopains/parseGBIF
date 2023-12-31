% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select_digital_voucher_v2.2.R
\name{select_digital_voucher_v2.2}
\alias{select_digital_voucher_v2.2}
\title{Selecting the master digital voucher}
\usage{
select_digital_voucher_v2.2(
  occ = NA,
  occ_gbif_issue = NA,
  occ_wcvp_check_name = NA,
  occ_collectorsDictionary = NA,
  enumOccurrenceIssue = NA,
  silence = TRUE
)
}
\arguments{
\item{occ}{GBIF occurrence table with selected columns as select_gbif_fields(columns = 'standard')}

\item{occ_gbif_issue}{= result of function extract_gbif_issue()$occ_gbif_issue}

\item{occ_wcvp_check_name}{= result of function batch_checkName_wcvp()$occ_wcvp_check_name}

\item{occ_collectorsDictionary}{= result of function update_collectorsDictionary()$occ_collectorsDictionary}

\item{enumOccurrenceIssue}{An enumeration of validation rules for single occurrence records by GBIF file, if NA, will be used, data(EnumOccurrenceIssue)}

\item{silence}{if TRUE does not display progress messages}
}
\value{
list with two data frames: occ_digital voucher_and:
occ_digital_voucher,  with all data processing fields and
occ_results, only result fields.
}
\description{
To group duplicates and choose the digital voucher:
Unique collection events can result in many ‘duplicate’ GBIF records. We designate one of these ‘duplicate’ records
as the master digital voucher, to which data from other duplicate vouchers can be merged (see export_data):

\strong{Where the collection event key for grouping duplicates is complete}, then duplicates can be grouped / parsed.
To do so, we evaluate record completeness. Record completeness is calculated based on data-quality scores
for the information in the following  fields: recordedBy, recordNumber, year, institutionCode, catalogNumber, locality, municipality,
countryCode, stateProvince and fieldNotes. The spatial coordinates associated with each duplicate are ranked using a score for the
quality of the geospatial information. This score is calculated using the issues listed in the GBIF table, EnumOccurrenceIssue.
A score is calculated based on these issues (see above). The duplicate with the highest total score is assigned as the master voucher
for the unique collection event. Missing information contained in duplicate records of the unique collection event can then be merged
into the master digital voucher (see export_data).

\strong{Where the collection event key is incomplete}, unique collection event duplicates cannot be parsed. In this case,
each record is considered as a unique collection event, without duplicates. However, to know the integrity
of the information, record completeness and quality of the geospatial information, are evaluated as described above.

\strong{How is the quality score calculated?}
parseGBIF_digital_voucher = The duplicate with the highest total score, sum of record completeness + quality of geospatial information.

\strong{How is record completeness calculated?}
The quality of the duplicate records associated with each collection event key is measured as the
completeness of a record, using the sum of a number of flags (see below) equal to TRUE.

\strong{Flags used to calculate record completeness}
\itemize{
\item Is there information about the collector?
\item Is there information about the collection number?
\item Is there information about the year of collection?
\item Is there information about the institution code?
\item Is there information about the catalog number?
\item Is there information about the locality?
\item Is there information about the municipality of collection?
\item Is there information about the state/province of collection?
\item Is there information about the field notes?
}

\strong{The quality of geospatial information is based on geographic issues raised by GBIF.}
GIBF issues relating to geospatial data were classified into three classes based on the data quality
scores that we assigned to each of the following GBIF issues recorded in the EnumOccurrenceIssue.
\itemize{
\item Issue does not affect coordinating accuracy, with selection_score equal to -1
\item Issue has potential to affect coordinate accuracy, with selection_score equal to -3
\item Records with a selection_score equal to -9 are excluded.
}
}
\details{
\itemize{
\item parseGBIF_duplicates_grouping_status - "groupable", "not groupable: no recordedBy and no recordNumber",
"not groupable: no recordNumber" or "not groupable: no recordedBy"
\item parseGBIF_num_duplicates number of duplicates records
\item parseGBIF_duplicates TRUE/FALSE
\item parseGBIF_non_groupable_duplicates TRUE/FALSE
}
}
\examples{
\donttest{
help(select_digital_voucher)

head(occ)
head(res_gbif_issue$occ_gbif_issue)
head(res_checkName_wcvp$occ_wcvp_check_name)
head(res_collectorsDictionary$occ_collectorsDictionary)
res_digital_voucher_and_sample_identification <- select_digital_voucher(occ = occ,
                                                                        occ_gbif_issue = res_gbif_issue$occ_gbif_issue,
                                                                        occ_wcvp_check_name = res_checkName_wcvp$occ_wcvp_check_name,
                                                                        occ_collectorsDictionary = res_collectorsDictionary$occ_collectorsDictionary,
                                                                        enumOccurrenceIssue = EnumOccurrenceIssue)

names(res_digital_voucher_and_sample_identification)

head(res_digital_voucher_and_sample_identification$occ_digital_voucher)
colnames(res_digital_voucher_and_sample_identification$occ_digital_voucher)

}
}
\seealso{
\code{\link[ParsGBIF]{batch_checkName_wcvp}}, \code{\link[ParsGBIF]{extract_gbif_issue}}
}
\author{
Pablo Hendrigo Alves de Melo,
Nadia Bystriakova &
Alexandre Monro
}
