# Data table definitions for the file, *ramp_crossref_unpaywall_merged.csv*

#### GitHub repository URL: <https://github.com/imls-measuring-up/ramp_citation_analysis>

Add description

Add data citations (Crossref, GSC, RAMP, Unpaywall)

## Column definitions and data sources

**ir**

> Data type: string

> Description: Each IR in RAMP is assigned two indices in Elasticsearch. These indices are described below, but for each IR both indices have a common prefix or root that is unique to that IR. This field is essentially a unique identifier for IR in RAMP, and is recommended for scripting batch processes across or independently of index type.

> Data source: RAMP

**unique_item_uri**

> Data type: string

> Description: A string identifier for an item held by the IR referenced in the **ir** column. This identifier is used to aggregate RAMP data about any content files associated with a single parent item. A parent item is defined as as any work, together with its metadata and corresponding content files, that has its own HTML landing page in an IR. Note that this URI is locally unique within the referenced IR. For an identifier that is globally unique across the dataset, the values of the **ir** column and this column should be combined.

> Data source: RAMP

**doi**

> Data type: string

> Description: A digital object identifier. Here, the DOI of the item referenced by the **ir** and **unique_item_uri** columns.

> Data source: DOIs for items in the dataset were created by Crossref. The DOIs themselves were extracted from item level metadata provided by the IR which hosts the item. Specifically, DOIs were extracted from the "content" attributes of HTML "meta" tags that were identified as containing DOIs.

**item_metadata_field_w_doi**

> Data type: string

> Description: The metadata field in the item record published by the host IR from which the DOI in the **doi** field was extracted. 

> Data source: Item level metadata provided by the IR which hosts the item. Specifically, this value is taken from the "name" attribute of an HTML "meta" tag.

**doi_id_method**

> Data type: string

> Description: The python regular expression method which was used to determine whether the field referenced in the **item_metadata_field_w_doi** column contained a DOI. Possible values are "match" and "search."

> Data source: Generated during data aggregation.

**ir_pub_year**

> Data type: date

> Description: The year the item was published in the IR.

> Data source: Item level metadata provided by the IR which hosts the item.

**ct_citations**

> Data type: integer

> Description: The number of citations received by the article with the DOI referenced in the **doi** column, as of data collection date of ???.

> Data source: Crossref

**cref_created_year**

> Data type: date

> Description: The value of the ??? field in the Crossref metadata for the DOI referenced in the **doi** column.

> Data source: Crossref

**ir_is_oa_loc**

> Data type: logical

> Description: TRUE or FALSE depending on whether Unpaywall listed the IR referenced in the **ir** column as an open access host of the article with the DOI referenced in the **doi** column, as of the data collection date of ???.

> Data source: Unpaywall

**ct_oa_copies**

> Data type: integer

> Description: Total number of OA copies available for the article with the DOI referenced in the **doi** column.

> Data source: Unpaywall

**ct_ir_oa_copies**

> Data type: integer

> Description: Number of OA copies available from IR for the article with the DOI referenced in the **doi** column. Note that Unpaywall categorizes all OA hosts as either "publisher" or "repository." All OA hosts in the dataset were manually assigned subcategories of "institutional," "disciplinary," "publisher," or "other" types of OA hosts. The value of this column is the number of DOIs which are available from OA hosts subcategorized as "institional."

> Data source: Unpaywall. 

**ct_dr_oa_copies**

> Data type: integer

> Description: Number of OA copies available from disciplinary repositories for the article with the DOI referenced in the **doi** column. Note that Unpaywall categorizes all OA hosts as either "publisher" or "repository." All OA hosts in the dataset were manually assigned subcategories of "institutional," "disciplinary," "publisher," or "other" types of OA hosts. The value of this column is the number of DOIs which are available from OA hosts subcategorized as "disciplinary."

> Data source: Unpaywall. 

**ct_pub_oa_copies**

> Data type: integer

> Description: Number of OA copies available via publisher-provided OA for the article with the DOI referenced in the **doi** column. Note that Unpaywall categorizes all OA hosts as either "publisher" or "repository." All OA hosts in the dataset were manually assigned subcategories of "institutional," "disciplinary," "publisher," or "other" types of OA hosts. The value of this column is the number of DOIs which are available from OA hosts subcategorized as "publisher."

> Data source: Unpaywall. 

**ct_other_oa_copies**

> Data type: integer

> Description: Number of OA copies available via other types of OA for the article with the DOI referenced in the **doi** column. Note that Unpaywall categorizes all OA hosts as either "publisher" or "repository." All OA hosts in the dataset were manually assigned subcategories of "institutional," "disciplinary," "publisher," or "other" types of OA hosts. The value of this column is the number of DOIs which are available from OA hosts subcategorized as "other."

> Data source: Unpaywall. 

**item_uri_sum_clicks**

> Data type: integer

> Description: The number of clicks received between Jan 1 - May 31, 2019, by content files associated with the items referenced by the **ir** and **unique_item_uri** columns. In cases where an item has multiple associated content files, clicks are aggregated to the item level using the **unique_item_uri**. "Clicks" relate to clicks from search engine result pages on URLs that point to content files hosted by IR. All search engine result pages included in this study are Google properties, specifically web search and Scholar.

> Data source: Google Search Console