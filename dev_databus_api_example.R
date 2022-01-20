#import requests
#import json
#import hashlib
#import sys
#from datetime import datetime
#from dataclasses import dataclass, field
#from typing import List

DATABUS_URI_BASE = "https://dev.databus.dbpedia.org"



as.jsonld <- function(context, x) {
#    account_name: str
#    id: str
#    label: str
#    title: str
#    comment: str
#    abstract: str
#    description: str
    

    group_uri = getTargetURIGroup(x)
    
    group_data_dict = list(
        "@context" = context,
        "@graph"= list(
            
                "@id" = x,
                "@type" = "dataid:Group",
                "label" = list("@value" = "bla", "@language" = "en")

        )
    )
    return(jsonlite::toJSON(group_data_dict))
    
}

getTargetURIGroup <- function(x) {
    return(paste0(DATABUS_URI_BASE ,x["account_name"], x["id"]))
}

getTargetURIDataID <- function(x) {
    return(paste0(DATABUS_URI_BASE, x["account_name"], x["group"], x["artifact"], x["version"]))
}

 

class DatabusFile:
    def __init__(self, uri: str, cvs: dict, file_ext: str, **kwargs):
        """Fetches the necessary information of a file URI for the deploy to the databus."""
        self.uri = uri
        self.cvs = cvs
        resp = requests.get(uri, **kwargs)
        if resp.status_code > 400:
            print(f"ERROR for {uri} -> Status {str(resp.status_code)}")

        self.sha256sum = hashlib.sha256(bytes(resp.content)).hexdigest()
        self.content_length = str(len(resp.content))
        self.file_ext = file_ext
        self.id_string = "_".join([f"{k}={v}" for k, v in cvs.items()]) + "." + file_ext


@dataclass
class DataVersion:
    account_name: str
    group: str
    artifact: str
    version: str
    title: str
    label: str
    publisher: str
    comment: str
    abstract: str
    description: str
    license: str
    databus_files: List[DatabusFile]
    issued: datetime = field(default_factory=datetime.now)
    context: str = "https://raw.githubusercontent.com/dbpedia/databus-git-mockup/main/dev/context.jsonld"

    def get_target_uri(self):

        return f"https://dev.databus.dbpedia.org/{self.account_name}/{self.group}/{self.artifact}/{self.version}"

    def __distinct_cvs(self) -> dict:

        distinct_cv_definitions = {}
        for dbfile in self.databus_files:
            for key, value in dbfile.cvs.items():

                if not key in distinct_cv_definitions:
                    distinct_cv_definitions[key] = {
                        "@type": "rdf:Property",
                        "@id": f"dataid-cv:{key}",
                        "rdfs:subPropertyOf": {"@id": "dataid:contentVariant"},
                    }
        return distinct_cv_definitions

    def __dbfiles_to_dict(self):

        for dbfile in self.databus_files:
            file_dst = {
                "@id": self.version_uri + "#" + dbfile.id_string,
                "file": self.version_uri + "/" + self.artifact + "_" + dbfile.id_string,
                "@type": "dataid:SingleFile",
                "formatExtension": dbfile.file_ext,
                "compression": "none",
                "downloadURL": dbfile.uri,
                "byteSize": dbfile.content_length,
                "sha256sum": dbfile.sha256sum,
                "hasVersion": self.version,
            }
            for key, value in dbfile.cvs.items():

                file_dst[f"dataid-cv:{key}"] = value

            yield file_dst

    def to_jsonld(self, **kwargs) -> str:
        self.version_uri = (
            f"{DATABUS_URI_BASE}/{account_name}/{group}/{artifact}/{version}"
        )
        self.data_id_uri = self.version_uri + "#Dataset"

        self.artifact_uri = (
            f"{DATABUS_URI_BASE}/{account_name}/{group}/{artifact}"
        )

        self.group_uri = f"{DATABUS_URI_BASE}/{account_name}/{group}"

        self.timestamp = self.issued.strftime("%Y-%m-%dT%H:%M:%SZ")

        data_id_dict = {
            "@context": self.context,
            "@graph": [
                {
                    "@type": "dataid:Dataset",
                    "@id": self.data_id_uri,
                    "version": self.version_uri,
                    "artifact": self.artifact_uri,
                    "group": self.group_uri,
                    "hasVersion": self.version,
                    "issued": self.timestamp,
                    "publisher": self.publisher,
                    "label": {"@value": self.label, "@language": "en"},
                    "title": {"@value": self.title, "@language": "en"},
                    "comment": {"@value": self.comment, "@language": "en"},
                    "abstract": {"@value": self.abstract, "@language": "en"},
                    "description": {"@value": self.description, "@language": "en"},
                    "license": {"@id": self.license},
                    "distribution": [d for d in self.__dbfiles_to_dict()],
                }
            ],
        }

        for _, named_cv_prop in self.__distinct_cvs().items():
            data_id_dict["@graph"].append(named_cv_prop)

        # add explicit artifact statement

        data_id_dict["@graph"].append({"@id": self.get_target_uri().rsplit("/", 1)[0], "@type": "dataid:Artifact"})

        # Explicit Version Statement
        data_id_dict["@graph"].append({"@id": self.get_target_uri(), "@type": "dataid:Version"})


        return json.dumps(data_id_dict)


deploy_to_dev_databus <- function(context, api_key, group, dataid) {
    
        message(paste0("Deploying Group}"))
        group = as.jsonld(context, group)
        resp = PUT(getTargetURIGroup(group), headers='{"X-API-Key": api_key, "Content-Type": "application/json"}', data = group)
        
        if (resp["status_code"] >= 400) {
#            print(f"Response: Status {resp.status_code}; Text: {resp.text}")
#            print(f"Problematic file:\n {submission_data}")
        }

        message(paste0("Deploying DataID"))
        dataid = as.jsonld(context, dataid)
        resp = PUT(getTargetURIGroup(dataid), headers='{"X-API-Key": api_key, "Content-Type": "application/json"}', data = dataid)
        
        if (resp["status_code"] >= 400) {
#            print(f"Response: Status {resp.status_code}; Text: {resp.text}")
#            print(f"Problematic file:\n {submission_data}")
        }
        

}

context <- "https://raw.githubusercontent.com/dbpedia/databus-git-mockup/main/dev/context.jsonld"

account_name <- "pik-piam"

group <- "general"

artifact <- "testartifact"

version <- "2021-05-09"

title <- "Test Title"


# currently its advised to use the internal webid found at https://dev.databus.dbpedia.org/{{user}}#settings
publisher <- paste0("https://dev.databus.dbpedia.org/", account_name, "#this")

label <- "Test Label"

comment <- "This is a short comment about the test."

abstract <- "This a short abstract for the dataset. Since this is only a test it is quite insignificant."

description <- "A bit longer description of the dataset."

license <- "http://this.is.a.license.uri.com/test"

files <- [
    DatabusFile(
        "https://yum-yab.github.io/data/databus-api-test/first/pizza-ont.owl",
        {"type": "ontology"},
        "owl",
    ),
    DatabusFile(
        "https://yum-yab.github.io/data/databus-api-test/first/Sample500.csv",
        {"type": "randomData"},
        "csv",
    ),
    DatabusFile(
        "https://openenergy-platform.org/api/v0/schema/supply/tables/wind_turbine_library/rows/",
        {"type": "turbineData", "extra": "external"},
        "json",
    ),
]

databus_version = DataVersion(
    account_name=account_name,
    group=group,
    artifact=artifact,
    version=version,
    title=title,
    publisher=publisher,
    label=label,
    comment=comment,
    abstract=abstract,
    description=description,
    license=license,
    databus_files=files,
)

databus_group = DataGroup(
    account_name=account_name,
    id=group,
    label="Test Group",
    title="Test Group",
    abstract="Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.",
    comment="Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.",
    description="Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.",
)

# For the new version deployed to dev.databus.dbpedia.org
# API KEY can be found or generated under https://dev.databus.dbpedia.org/{{user}}#settings
deploy_to_dev_databus(account_name, "dfb7c483-a2af-4c79-ae39-b8975d79ccd4", databus_group, databus_version)
