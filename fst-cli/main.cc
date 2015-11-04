
#include <fstlib.h>
#include <fstream>

using namespace std;

void usage()
{
    cout << R"(usage: fst-cli <command> [<args>]

    compile  DICTIONARY INDEX
    dot      DICTIONARY
    dump     INDEX
    search   INDEX
)";
}

vector<pair<string, string>> load_input(istream& fin)
{
    cerr << "# loading dictionary..." << endl;
    vector<pair<string, string>> input;
    string word;
    while (getline(fin, word)) {
        input.emplace_back(word, to_string(input.size()));
    }

    cerr << "# sorting dictionary..." << endl;
    sort(input.begin(), input.end(), [](const auto& a, const auto& b) {
        return a.first < b.first;
    });

    return input;
}

shared_ptr<fst::State> get_state_machine(const vector<pair<string, string>>& input)
{
    cerr << "# making fst..." << endl;
    auto initial_state = fst::make_state_machine(input);
    cerr << "# state count: " << initial_state->id + 1 << endl;

    return initial_state;
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

    string cmd = argv[argi++];

    if (cmd == "compile") {
        if (argi >= argc) {
            usage();
            return 1;
        }

        ifstream fin(argv[argi++]);
        if (!fin) {
            return 1;
        }

        if (argi >= argc) {
            usage();
            return 1;
        }

        ofstream fout(argv[argi++], ios_base::binary);
        if (!fout) {
            return 1;
        }

        auto input = load_input(fin);
        auto initial_state = get_state_machine(input);

        cerr << "# compile fst..." << endl;
        auto byte_code = fst::compile(initial_state);
        cerr << "# byte code size: " << byte_code.size() << endl;

        fout.write(byte_code.data(), byte_code.size());

    } else if (cmd == "dot") {
        if (argi >= argc) {
            usage();
            return 1;
        }

        ifstream fin(argv[argi++]);
        if (!fin) {
            return 1;
        }

        auto input = load_input(fin);
        auto initial_state = get_state_machine(input);
        initial_state->dot(cout);

    } else if (cmd == "dump") {
        if (argi >= argc) {
            usage();
            return 1;
        }

        ifstream fin(argv[argi++], ios_base::binary);
        if (!fin) {
            return 1;
        }

        fst::dump(load_byte_code(fin), cout);

    } else if (cmd == "search") {
        if (argi >= argc) {
            usage();
            return 1;
        }

        ifstream fin(argv[argi++], ios_base::binary);
        if (!fin) {
            return 1;
        }

        fin.seekg(0, ios_base::end);
        auto size = fin.tellg();
        fin.seekg(0, ios_base::beg);
        vector<char> byte_code(size);
        fin.read(byte_code.data(), size);
        cerr << "byte code size: " << byte_code.size() << endl;

        for (;;) {
            string word;
            cin >> word;
            auto results = fst::search(byte_code, word);
            cout << "results: " << results.size() << endl;
            for (const auto& item : results) {
                cout << item << endl;
            }
        }
    } else if (cmd == "test") {
        if (argi >= argc) {
            usage();
            return 1;
        }

        ifstream fin(argv[argi++]);
        if (!fin) {
            return 1;
        }

        auto input = load_input(fin);
        auto initial_state = get_state_machine(input);
        auto byte_code = fst::compile(initial_state);

        //fst::dump(byte_code, cout);

        for (const auto& item: input) {
            auto results = fst::search(initial_state, item.first);
            if (results.empty()) {
                cout << item.first << ": ng" << endl;
            } else {
                //cout << item.first << ": ok" << endl;
            }

            results = fst::search(byte_code, item.first);
            if (results.empty()) {
                cout << item.first << ": NG" << endl;
            } else {
                //cout << item.first << ": OK" << endl;
            }
        }

    } else {
        usage();
        return 1;
    }

    return 0;
}
