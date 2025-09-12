defmodule Livebook.FileSystemHelpers do
  def s3_id(hub_id, bucket_url) do
    Livebook.FileSystem.Utils.id("s3", hub_id, bucket_url)
  end
end
