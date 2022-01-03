# Data table definitions for the file, *ramp_crossref_unpaywall_merged.csv*

#### GitHub repository URL: <https://github.com/imls-measuring-up/ramp_citation_analysis>

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

> Description: A digital object identifier. 

> Data source: DOIs for items in the dataset were created by Crossref. The DOIs themselves were extracted from item level metadata provided by the IR which hosts the item. Specifically, DOIs were extracted from the "content" attributes of HTML "meta" tags that were identified as containing DOIs.

**item_metadata_field_w_doi**

> Data type: string

> Description: The metadata field in the item record published by the host IR from which the DOI in the **doi** field was extracted. 

> Data source: Item level metadata provided by the IR which hosts an item. Specifically, this value is taken from the "name" attribute of an HTML "meta" tag.

**doi_id_method**

> Data type: string

> Description: The python regular expression method which was used to determine whether the field referenced in the **item_metadata_field_w_doi** column contained a DOI. Possible values are "match" and "search."

> Data source: Generated during data aggregation.

**ir_pub_year**

> Data type: date

