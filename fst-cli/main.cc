
#include <fstlib.h>
#include <fstream>
#include <sstream>

using namespace std;

void usage()
{
    cout << R"(usage: fst-cli <command> [<args>]

    compile     DictionaryFile FstFile     - make fst byte code

    search      FstFile                    - exact match search
    prefix      FstFile                    - common prefix search

    dot         DictionaryFile             - convert to dot format
    assembly    DictionaryFile             - convert to assembly format
)";
}

vector<pair<string, string>> load_input(istream& fin)
{
    vector<pair<string, string>> input;

    string word;
    while (getline(fin, word)) {
        input.emplace_back(word, to_string(input.size()));
    }

    sort(input.begin(), input.end(), [](const auto& a, const auto& b) {
        return a.first < b.first;
    });

    return input;
}

vector<char> load_byte_code(istream& is)
{
    is.seekg(0, ios_base::end);
    auto size = is.tellg();
    is.seekg(0, ios_base::beg);
    vector<char> byte_code(size);
    is.read(byte_code.data(), size);
    return byte_code;
}

int main(int argc, const char** argv)
{
    int argi = 1;

    if (argi >= argc) {
        usage();
        return 1;
    }

    try {
        string cmd = argv[argi++];

        if (cmd == "compile") {
            if (argi >= argc) {
                usage();
                return 1;
            }

            ifstream fin(argv[argi++]);
            if (!fin) { usage(); return 1; }

            if (argi >= argc) { usage(); return 1; }

            ofstream fout(argv[argi++], ios_base::binary);
            if (!fout) { usage(); return 1; }

            auto initial_state = fst::make_state_machine(load_input(fin));
            auto byte_code = fst::compile(initial_state);
            fout.write(byte_code.data(), byte_code.size());

        } else if (cmd == "search" || cmd == "prefix") {
            if (argi >= argc) { usage(); return 1; }

            ifstream fin(argv[argi++], ios_base::binary);
            if (!fin) { usage(); return 1; }

            fin.seekg(0, ios_base::end);
            auto size = fin.tellg();
            fin.seekg(0, ios_base::beg);
            vector<char> byte_code(size);
            fin.read(byte_code.data(), size);

            string line;
            while (getline(cin, line)) {
                if (cmd == "search") {
                    auto outputs = fst::exact_match_search(byte_code, line);
                    for (const auto& item : outputs) {
                        cout << item << endl;
                    }
                } else { // "prefix"
                    auto results = fst::common_prefix_search(byte_code, line);
                    for (const auto& result : results) {
                        cout << "length: " << result.length << endl;
                        for (const auto& item : result.outputs) {
                            cout << item << endl;
                        }
                    }
                }
            }

        } else if (cmd == "dot") {
            if (argi >= argc) { usage(); return 1; }

            ifstream fin(argv[argi++]);
            if (!fin) { usage(); return 1; }

            auto initial_state = fst::make_state_machine(load_input(fin));
            initial_state->dot(cout);

        } else if (cmd == "assembly") {
            if (argi >= argc) { usage(); usage(); return 1; }

            ifstream fin(argv[argi++]);
            if (!fin) { usage(); return 1; }

            auto initial_state = fst::make_state_machine(load_input(fin));
            fst::print(initial_state, cout);

        } else if (cmd == "test") {
            if (argi >= argc) { usage(); return 1; }

            ifstream fin(argv[argi++]);
            if (!fin) { usage(); return 1; }

            cerr << "# loading dictionary..." << endl;
            auto input = load_input(fin);

            cerr << "# making fst..." << endl;
            auto initial_state = fst::make_state_machine(input);
            cerr << "# state count: " << initial_state->id + 1 << endl;

            cerr << "# compile fst..." << endl;
            auto byte_code = fst::compile(initial_state);
            cerr << "# byte code size: " << byte_code.size() << endl;

            cerr << "# test all words..." << endl;
            for (const auto& item: input) {
                auto results = fst::exact_match_search(byte_code, item.first);
                if (results.empty()) {
                    cout << item.first << ": NG" << endl;
                }
            }
        } else {
            usage();
            return 1;
        }
    } catch (const runtime_error& err) {
        cerr << err.what() << endl;
    }

    return 0;
}
