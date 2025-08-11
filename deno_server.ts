import { serveDir, serveFile } from 'jsr:@std/http/file-server';

const INDEX_HTML_PATH = './docs/index.html'

Deno.serve((req) => {
  const pathName = new URL(req.url).pathname;
  if (pathName === '/') {
    return serveFile(req, INDEX_HTML_PATH);
  } else if (pathName.startsWith('/public')) {
    return serveDir(req, {
      fsRoot: './docs/public',
      urlRoot: 'public',
    });
  } else {
    return new Response("404: not found", { status: 404 });
  }
});