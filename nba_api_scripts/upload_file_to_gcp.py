import os
from dotenv import load_dotenv
from google.cloud import storage

from get_logger import get_logger

load_dotenv()
logger = get_logger(__name__)


def upload_file_to_gcp(
    source_file_name, sink_blob_name, bucket_name=os.getenv("GCP-BUCKET")
):
    logger.info("Connecting to GCP Storage")
    client = storage.Client()
    # https://console.cloud.google.com/storage/browser/[bucket-id]/
    bucket = client.get_bucket(bucket_name)
    blob = bucket.blob(sink_blob_name)
    logger.info("Starting file upload")
    blob.upload_from_filename(filename=source_file_name)
    logger.info(f"File {source_file_name} uploaded to {sink_blob_name}.")
