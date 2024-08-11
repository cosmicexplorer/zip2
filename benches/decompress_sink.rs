#![cfg(all(feature = "_deflate-any", feature = "bzip2"))]

use bencher::{benchmark_group, benchmark_main};

use std::fs;
use std::io::prelude::*;
use std::path::Path;

use bencher::Bencher;

use zip::read::stream::{ZipStreamFileMetadata, ZipStreamReader, ZipStreamVisitor};
use zip::read::ZipFile;
use zip::unstable::read::streaming::StreamingArchive;
use zip::{result::ZipResult, ZipArchive};

/* This contains the compressed text of King Lear from Project Gutenberg, in the public domain. */
fn get_test_data() -> ZipResult<ZipArchive<fs::File>> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/data/king-lear-compressed.zip");
    let file = fs::File::open(path)?;
    ZipArchive::new(file)
}

fn write_entry_to_sink_generic(bench: &mut Bencher) {
    let mut archive = get_test_data().unwrap();
    let total_size: u64 = archive.decompressed_size().unwrap().try_into().unwrap();

    bench.bytes = total_size;
    bench.bench_n(1, |bench| {
        bench.iter(|| {
            for i in 0..archive.len() {
                let mut f = archive.by_index_generic(i).unwrap();
                std::io::copy(&mut f, &mut std::io::sink()).unwrap();
            }
        })
    });
}

fn write_entry_to_sink_standard(bench: &mut Bencher) {
    let mut archive = get_test_data().unwrap();
    let total_size: u64 = archive.decompressed_size().unwrap().try_into().unwrap();

    bench.bytes = total_size;
    bench.bench_n(1, |bench| {
        bench.iter(|| {
            for i in 0..archive.len() {
                let mut f = archive.by_index(i).unwrap();
                std::io::copy(&mut f, &mut std::io::sink()).unwrap();
            }
        })
    });
}

fn write_stream_to_sink_generic(bench: &mut Bencher) {
    let archive = get_test_data().unwrap();
    let total_size: u64 = archive.decompressed_size().unwrap().try_into().unwrap();

    let mut reader = archive.into_inner();

    bench.bytes = total_size;
    bench.bench_n(1, |bench| {
        bench.iter(|| {
            reader.rewind().unwrap();
            let mut stream_zip = StreamingArchive::new(&mut reader);

            while let Some(mut file) = stream_zip.next_entry().unwrap() {
                std::io::copy(&mut file, &mut std::io::sink()).unwrap();
            }
            while stream_zip.next_metadata_entry().unwrap().is_some() {}
        })
    });
}

fn write_stream_to_sink_standard(bench: &mut Bencher) {
    let archive = get_test_data().unwrap();
    let total_size: u64 = archive.decompressed_size().unwrap().try_into().unwrap();

    struct V;
    impl ZipStreamVisitor for V {
        fn visit_file(&mut self, file: &mut ZipFile) -> ZipResult<()> {
            std::io::copy(file, &mut std::io::sink())?;
            Ok(())
        }
        fn visit_additional_metadata(
            &mut self,
            _metadata: &ZipStreamFileMetadata,
        ) -> ZipResult<()> {
            Ok(())
        }
    }

    let mut reader = archive.into_inner();

    bench.bytes = total_size;
    bench.bench_n(1, |bench| {
        bench.iter(|| {
            reader.rewind().unwrap();
            let stream_zip = ZipStreamReader::new(&mut reader);

            stream_zip.visit(&mut V).unwrap();
        })
    });
}

benchmark_group!(
    benches,
    write_entry_to_sink_generic,
    write_entry_to_sink_standard,
    write_stream_to_sink_generic,
    write_stream_to_sink_standard,
);

benchmark_main!(benches);
