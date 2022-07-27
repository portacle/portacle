class Portacle < Formula
  desc "A portable Common Lisp development environment"
  homepage "https://portacle.github.io"
  license "zlib"
  head "https://github.com/portacle/portacle.git"

  depends_on "autoconf" => :build
  depends_on "automake" => :build
  depends_on "sbcl" => :build
  depends_on "git" => :build
  depends_on "gpg" => :build
  depends_on "gettext" => :build
  depends_on "pcre" => :build
  depends_on "openssl" => :build
  depends_on "libtool" => :build
  depends_on "gnu-sed" => :build
  depends_on "glfw" => :build
  depends_on "libgcrypt" => :build
  depends_on "pkg-config" => :build
  depends_on "xz" => :build
  depends_on "automake" => :build
  depends_on "gnutls" => :build
  depends_on "pcre2" => :build
  depends_on "texinfo" => :build
  depends_on "gnutls" => :build
  depends_on "go" => :build
  depends_on "python3" => :build

  def install
    ENV.deparallelize
    cd "build" do
      system "./build.sh", "build"
    end
  end

  test do
    # `test do` will create, run in and delete a temporary directory.
    #
    # This test will fail and we won't accept that! For Homebrew/homebrew-core
    # this will need to be a test that verifies the functionality of the
    # software. Run the test with `brew test portacle`. Options passed
    # to `brew install` such as `--HEAD` also need to be provided to `brew test`.
    #
    # The installed folder is not in the path, so use the entire path to any
    # executables being tested: `system "#{bin}/program", "do", "something"`.
    system "false"
  end
  end
