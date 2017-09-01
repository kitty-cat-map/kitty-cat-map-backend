
CREATE TABLE IF NOT EXISTS image_info (
  id SERIAL CONSTRAINT firstkey PRIMARY KEY,
  filename TEXT
  );

-- Create the geom column on the image_info table.
SELECT AddGeometryColumn ('image_info','geom',4326,'POINT',2);
